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
  * All methods required to update the Node View of Stakeholder to the latest global slot provided by Coordinator
  */

trait Update extends Members {
  import Parameters.{printFlag,epochLength,useGossipProtocol,numGossipers,dataOutInterval,dataOutFlag,useFencing}

  /**epoch routine, called every time currentEpoch increments*/
  def updateEpoch(slot:Slot,epochIn:Int,lastEta:Eta,chain:Tine):(Int,Eta) = {
    val ep = slot / epochLength
    if (ep > epochIn) {
      val newEta = eta_from_tine(chain, ep, lastEta)
      (epochIn+1,newEta)
    } else {
      (epochIn,lastEta)
    }
  }

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

  def update:Unit = timeFlag{
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
          case result:(Int,Eta) if result._1 > currentEpoch => {
            currentEpoch = result._1
            eta = result._2
            stakingState = getStakingState(currentEpoch,localChain)
            alphaCache match {
              case Some(loadingCache:LoadingCache[ByteArrayWrapper,Ratio]) => {
                loadingCache.invalidateAll()
              }
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
        maxValidBG
      }
      if (useFencing) roundBlock match {
        case 0 => {
          forgeBlock(keys)
          roundBlock = 1
          routerRef ! Flag(selfWrapper,"updateSlot")
        }
        case _ if chainUpdateLock => {
          if (tinePoolWithPrefix.isEmpty) {
            chainUpdateLock = false
          } else {
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Checking Tine")
            }
            maxValidBG
          }
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
          out
        }
        for (holder<-holders) {
          inbox.toList.find(info=>info._2._1==holder) match {
            case Some(inboxInfo) => Try{
              val hpk:PublicKeyW = ByteArrayWrapper(inboxInfo._2._2._1++inboxInfo._2._2._2++inboxInfo._2._2._3)
              val str = holder.actorPath.toString
              wallet.confirmedState.get(hpk) match {
                case Some(st) => {
                  val ha = relativeStake(hpk,wallet.confirmedState).toDouble
                  SharedData.confirmedBalance = SharedData.confirmedBalance + (str->st._1)
                  SharedData.confirmedAlpha = SharedData.confirmedAlpha + (str->ha)
                }
                case None =>
              }
              stakingState.get(hpk) match {
                case Some(st) => {
                  val ha = relativeStake(hpk,stakingState).toDouble
                  SharedData.stakingBalance = SharedData.stakingBalance + (str->st._1)
                  SharedData.stakingAlpha = SharedData.stakingAlpha + (str->ha)
                }
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
            case Some(st) => {
              val ha = relativeStake(hpk,wallet.confirmedState).toDouble
              SharedData.confirmedBalance = SharedData.confirmedBalance + (str->st._1)
              SharedData.confirmedAlpha = SharedData.confirmedAlpha + (str->ha)
            }
            case None =>
          }
          stakingState.get(hpk) match {
            case Some(st) => {
              val ha = relativeStake(hpk,stakingState).toDouble
              SharedData.stakingBalance = SharedData.stakingBalance + (str->st._1)
              SharedData.stakingAlpha = SharedData.stakingAlpha + (str->ha)
            }
            case None =>
          }
        }
      }
      updating = false
    }
  }

}
