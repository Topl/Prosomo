package prosomo.stakeholder

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.{Flag, Hello, WriteFile}
import prosomo.components.Tine
import prosomo.primitives.Parameters.useGui
import prosomo.primitives.{KeyFile, Parameters, Ratio, SharedData}
import scorex.util.encode.Base58
import scala.util.Try

/**
  * AMS 2020:
  * All methods required to update the Node View of Stakeholder to the latest global slot provided by Coordinator
  */

trait Update extends Members {
  import Parameters.{printFlag,epochLength,useGossipProtocol,numGossipers,dataOutInterval,dataOutFlag,useFencing}

  /**
    * Epoch update routine, called every time currentEpoch increments, calculates the new epoch nonce
    * @param slot the slot being tested for update
    * @param epochIn the epoch number before slot is tested
    * @param lastEta the epoch nonce before slot is tested
    * @param chain the tine with vrf nonces to apply
    * @return epoch after slot is tested, epoch nonce after slot is tested
    */
  def updateEpoch(slot:Slot,epochIn:Int,lastEta:Eta,chain:Tine):(Int,Eta) = {
    val ep = slot / epochLength
    if (ep > epochIn) {
      val newEta = eta_from_tine(chain, ep, lastEta)
      (epochIn+1,newEta)
    } else {
      (epochIn,lastEta)
    }
  }

  /*********************************************************************************************************************
    * The staking distribution that is used to calculate the epoch relative stake,
    * Used in the staking procedure, the previous epoch is ep-1,
    * This staking distribution is sampled form the state at the beginning of the previous epoch or the genesis block,
    * This staking state results from all modifiers applied from the epoch leading into the previous epoch, i.e. ep-2,
    * The routine gets the last active slot starting at the dawn slot of the previous epoch,
    *
    * Note AMS 2020: the current implementation may include a block forged in ep-1 in the dawn slot,
    * i.e. the 1st slot of ep-1
    * As far as I can see, this presents no security vulnerability since any block in the dawn slot contains modifiers
    * only from the past epoch.
    * The dawn slot block would have been forged with epoch parameters in ep-1 that are already
    * well established from ep-2 and cannot be altered by any
    * present modifier or malicious tine assuming honest majority.
    *
    * The difference between this implementation and taking the staking distribution
    * from the dusk (last) slot of ep-2 is described above.
    * The reasoning behind this implementation is that the stake distribution is sampled from
    * the 1st slot (slot=0) of epoch 0 initially,
    * so if we keep sampling from the dawn slot of each epoch during updates the interval between
    * stake distributions is constant throughout the entire protocol.
    *
    * @param ep epoch number corresponding to the returned staking distribution
    * @param chain tine containing the block ids of at least the previous epoch
    * @return the staking distribution to be used in epoch number ep
    */
  def getStakingState(ep:Int, chain:Tine):State = if (ep > 1) {
    val eps:Slot = (ep-1)*epochLength
    history.get(chain.getLastActiveSlot(eps)) match {
      case Some(value:(State,Eta)) =>
        value._1
      case _ =>
        val thisSlot = lastActiveSlot(chain,eps)
        println(s"Could not recover staking state ep $ep slot $thisSlot id:"+Base58.encode(localChain.getLastActiveSlot(eps)._2.data))
        chain.print
        SharedData.throwError(holderIndex)
        Map()
    }
  } else {
    history.get(chain.get(0)) match {
      case Some(value:(State,Eta)) =>
        value._1
      case _ =>
        println("Could not recover staking state ep 0")
        SharedData.throwError(holderIndex)
        Map()
    }
  }


  /*********************************************************************************************************************
    * The main update procedure that carries out consensus and forges, by default carried out 100 times a second
    *
    * localSlot is used to keep track of epoch updates,
    * it updates to globalSlot in a while loop and triggers updateEpoch,
    * this allows the protocol to execute under stalls that delay queries to the Coordinator for time updates.
    *
    * Once local slot has reached global slot, and all epoch variables are updated, update the key to global slot.
    *
    * The staking procedure (testing the nonce if then forging) is carried out
    * even if the current set of keys has no stake, there is virtually no cost to forging
    * so it always occurs if active stake is held by the keys.
    * Newly forged blocks are added to the end of tinePoolWithPrefix so MaxValid-BG will adopt it first
    *
    * The new key is saved to disk, the keys already on disk are made old keys, and the old old key file is erased,
    * leaving only new key and old key on disk.
    *
    * Gossipers are requested and updated if there are less than the desired number of gossipers
    *
    * Chain selection according to MaxValid-BG occurs on the last element of tinePoolWithPrefix
    *
    * Aux information is updated
    */

  def update():Unit = timeFlag{
    if (SharedData.error) actorStalled = true
    if (!actorStalled && !updating) {
      updating = true
      if (SharedData.killFlag) {
        timers.cancelAll
        updating = false
      }
      while (globalSlot > localSlot) {
        localSlot += 1
        if (dataOutFlag && localSlot % dataOutInterval == 0) {
          coordinatorRef ! WriteFile
        }
        updateEpoch(localSlot,currentEpoch,eta,localChain) match {
          case result:(Int,Eta) if result._1 > currentEpoch =>
            currentEpoch = result._1
            eta = result._2
            stakingState = getStakingState(currentEpoch,localChain)
            alphaCache match {
              case Some(loadingCache:LoadingCache[ByteArrayWrapper,Ratio]) =>
                loadingCache.invalidateAll()
              case None => alphaCache = Some(
                CacheBuilder.newBuilder().build[ByteArrayWrapper,Ratio](
                  new CacheLoader[ByteArrayWrapper,Ratio] {
                    def load(id:ByteArrayWrapper):Ratio = {relativeStake(id,stakingState)}
                  }
                )
              )
            }
            thresholdCache match {
              case Some(loadingCache: LoadingCache[(Ratio,Slot),Ratio]) => loadingCache.invalidateAll()
              case None =>
            }
            keys.alpha = alphaCache.get.get(keys.pkw)
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Current Epoch = " + currentEpoch.toString)
              println("Holder " + holderIndex.toString + " alpha = " + keys.alpha.toDouble+"\nEta:"+Base58.encode(eta))
            }
          case _ =>
        }
        if (globalSlot == localSlot && updating) {
          val keyTime = keys.sk_kes.time(kes)

          if (keyTime < globalSlot) {
            keys.sk_kes.update_fast(kes, globalSlot)
          }

          if (holderIndex == SharedData.printingHolder) println(Console.CYAN + "Slot = " + localSlot.toString + " on block "
            + Base58.encode(localChain.getLastActiveSlot(localSlot)._2.data) + Console.RESET)
          if (!useFencing) {
            forgeBlock(keys)
          }
          keyFile = Some(KeyFile.update(keyFile.get,keys.sk_kes,password,keyDir,serializer,salt,derivedKey))
          if (useGossipProtocol) {
            val newOff = (numGossipers*math.sin(2.0*math.Pi*(globalSlot.toDouble/100.0+phase))/2.0).toInt
            if (newOff != gOff) {
              if (gOff < newOff) numHello = 0
              gOff = newOff
            }
            if (gossipers.length < numGossipers + gOff && numHello < 1) {
              send(selfWrapper,rng.shuffle(holders.filter(_!=selfWrapper)),Hello(selfWrapper,signMac(hash(selfWrapper,serializer), sessionId, keys.sk_sig, keys.pk_sig)))
              numHello += 1
            } else if (gossipers.length > numGossipers + gOff) {
              gossipers = rng.shuffle(gossipers).take(numGossipers + gOff)
            }
          }
        }
      }
      if (!useFencing) while (tinePoolWithPrefix.nonEmpty && updating) {
        maxValidBG()
      }
      if (useFencing) roundBlock match {
        case 0 =>
          forgeBlock(keys)
          roundBlock = 1
          routerRef ! Flag(selfWrapper,"updateSlot")
        case _ if chainUpdateLock =>
          if (tinePoolWithPrefix.isEmpty) {
            chainUpdateLock = false
          } else {
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Checking Tine")
            }
            maxValidBG()
          }
        case _ =>
      }
      if (holderIndex == SharedData.printingHolder && useGui) {
        SharedData.walletInfo = (wallet.getNumPending,wallet.getConfirmedTxCounter,wallet.getConfirmedBalance,wallet.getPendingBalance)
        SharedData.issueTxInfo = Some((keys.pkw,inbox))
        SharedData.selfWrapper = Some(selfWrapper)
        SharedData.blockTime = {
          val head = localChain.getLastActiveSlot(globalSlot)
          globalSlot.toDouble/getBlockHeader(head).get._9.toDouble
        }
        SharedData.activeSlots = 1.0/SharedData.blockTime
        SharedData.numTxsMempool = memPool.keySet.size
        SharedData.txsPerSecond = {
          var net = 0
          for (entry<-localState.toSeq) {
            net += entry._2._3
          }
          net.toDouble/globalSlot.toDouble
        }
        SharedData.activePeers = holders.size
        SharedData.activeStake = {
          var out = 0.0
          for (info<-inbox) {
            out += relativeStake(ByteArrayWrapper(info._2._2._1++info._2._2._2++info._2._2._3),stakingState).toDouble
          }
          out += relativeStake(keys.pkw,stakingState).toDouble
          out
        }
        for (holder<-holders) {
          inbox.toList.find(info=>info._2._1==holder) match {
            case Some(inboxInfo) => Try{
              val hpk:PublicKeyW = ByteArrayWrapper(inboxInfo._2._2._1++inboxInfo._2._2._2++inboxInfo._2._2._3)
              val str = holder.actorPath.toString
              wallet.confirmedState.get(hpk) match {
                case Some(st) =>
                  val ha = relativeStake(hpk,wallet.confirmedState).toDouble
                  SharedData.confirmedBalance = SharedData.confirmedBalance + (str->st._1)
                  SharedData.confirmedAlpha = SharedData.confirmedAlpha + (str->ha)
                case None =>
              }
              stakingState.get(hpk) match {
                case Some(st) =>
                  val ha = relativeStake(hpk,stakingState).toDouble
                  SharedData.stakingBalance = SharedData.stakingBalance + (str->st._1)
                  SharedData.stakingAlpha = SharedData.stakingAlpha + (str->ha)
                case None =>
              }
            }
            case None =>
          }
        }
        Try{
          val hpk:PublicKeyW = keys.pkw
          val str = selfWrapper.actorPath.toString
          wallet.confirmedState.get(hpk) match {
            case Some(st) =>
              val ha = relativeStake(hpk,wallet.confirmedState).toDouble
              SharedData.confirmedBalance = SharedData.confirmedBalance + (str->st._1)
              SharedData.confirmedAlpha = SharedData.confirmedAlpha + (str->ha)
            case None =>
          }
          stakingState.get(hpk) match {
            case Some(st) =>
              val ha = relativeStake(hpk,stakingState).toDouble
              SharedData.stakingBalance = SharedData.stakingBalance + (str->st._1)
              SharedData.stakingAlpha = SharedData.stakingAlpha + (str->ha)
            case None =>
          }
        }
      }
      updating = false
    }
  }

}
