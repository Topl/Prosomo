package prosomo.stakeholder

import akka.actor.Cancellable
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.{BootstrapJob, RequestBlock, RequestTine, SendTx}
import prosomo.components.{Tine, Transaction}
import prosomo.primitives.{Parameters, Ratio, SharedData}
import scorex.util.encode.Base58
import scala.concurrent.duration._

import scala.util.Try
import scala.util.control.Breaks.{break, breakable}

/**
  * AMS 2020:
  * Primary consensus routines,
  * Implements the Maxvalid-BG chain selection rule specified in Ouroboros Genesis
  *
  * Tinepool functionality is executed in these methods,
  * when a common ancestor is not found for a block a request is made for the block id and sent to the network.
  *
  * Stateful validation and ledger accumulation is executed here,
  * Txs are filed into MemPool from every block encountered.
  */

trait ChainSelection extends Members {
  import Parameters._

  def updateWallet():Unit = Try{
    var id = localChain.getLastActiveSlot(globalSlot)
    val bn:Int = getBlockHeader(id).get._9
    if (bn == 0) {
      wallet.update(history.get(id).get._1)
      if (holderIndex == SharedData.printingHolder) {
        SharedData.walletInfo = (
          wallet.getNumPending,
          wallet.getConfirmedTxCounter,
          wallet.getConfirmedBalance,
          wallet.getPendingBalance
        )
        SharedData.issueTxInfo = Some((keys.pkw,inbox))
        SharedData.selfWrapper = Some(selfWrapper)
      }
    } else {
      breakable{
        while (true) {
          id = getParentId(id).get
          getBlockHeader(id) match {
            case Some(b:BlockHeader) =>
              val bni = b._9
              if (bni <= bn-confirmationDepth || bni == 0) {
                wallet.update(history.get(id).get._1)
                if (holderIndex == SharedData.printingHolder) {
                  SharedData.walletInfo = (
                    wallet.getNumPending,
                    wallet.getConfirmedTxCounter,
                    wallet.getConfirmedBalance,
                    wallet.getPendingBalance
                  )
                  SharedData.issueTxInfo = Some((keys.pkw,inbox))
                  SharedData.selfWrapper = Some(selfWrapper)
                }
                break
              }
            case None =>
              println("Error: invalid id in wallet")
              break
          }
        }
      }
    }
    for (trans:Transaction <- wallet.getPending(localState)) {
      if (!memPool.keySet.contains(trans.sid)) memPool += (trans.sid->(trans,0))
      send(selfWrapper,gossipSet(selfWrapper,holders), SendTx(trans,selfWrapper))
    }

    def collectStake():Unit = Try{
      for (entry<-wallet.confirmedState) if (!wallet.reallocated.keySet.contains(entry._1)) {
        if (wallet.isSameLedgerId(entry._1) && entry._2._1 > 0) {
          wallet.issueTx(entry._1,wallet.pkw,entry._2._1,keys.sk_sig,sig,rng,serializer) match {
            case Some(trans:Transaction) =>
              if (holderIndex == SharedData.printingHolder && printFlag)
                println("Holder " + holderIndex.toString + " Reallocated Stake")
              txCounter += 1
              memPool += (trans.sid->(trans,0))
              send(selfWrapper,gossipSet(selfWrapper,holders), SendTx(trans,selfWrapper))
              wallet.reallocated += (entry._1->trans.nonce)
            case _ =>
          }
        }
      }
    }

    collectStake()
    walletStorage.store(wallet,serializer)
  }

  def buildTine(job:(Int,(Tine,Int,Int,Int,ActorRefWrapper))): Unit = {
    val entry = job._2
    var foundAncestor = true
    var tine:Tine = Tine(entry._1)
    var counter:Int = entry._2
    val previousLen:Int = entry._3
    var totalTries:Int = entry._4
    val ref:ActorRefWrapper = entry._5
    var prefix:Slot = 0
    breakable{
      while(foundAncestor) {
        getParentId(tine.least) match {
          case Some(parentId:SlotId) =>
            getBlockHeader(parentId) match {
              case Some(pbh:BlockHeader) =>
                tine = Tine(parentId,pbh._5) ++ tine
                if (tine.least == localChain.get(tine.least._1)) {
                  prefix = tine.least._1
                  tine.remove(prefix)
                  break
                }
                if (tine.least._1 == 0) {
                  prefix = 0
                  tine.remove(prefix)
                  break
                }
              case None =>
                val tineLength = getActiveSlots(tine)
                if (tineLength>tineMaxDepth && job._1 >= 0 && !helloLock) {
                  bootStrapLock = true
                  bootStrapJob = job._1
                  if (holderIndex == SharedData.printingHolder && printFlag) println(
                    "Holder " + holderIndex.toString
                      + " Looking for Parent Tine, Job:"+job._1
                      +" Tries:"+counter.toString+" Length:"+tineLength+" Tines:"+tinePool.keySet.size
                  )
                  val depth:Int = if (tineLength < tineMaxDepth) {
                    tineLength
                  } else {
                    tineMaxDepth
                  }
                  send(selfWrapper,ref, RequestTine(parentId,depth,job._1,selfWrapper))
                } else {
                  if (holderIndex == SharedData.printingHolder && printFlag) println(
                    "Holder " + holderIndex.toString
                      + " Looking for Parent Block, Job:"+job._1
                      +" Tries:"+counter.toString+" Length:"+getActiveSlots(tine)+" Tines:"+tinePool.keySet.size
                  )
                  send(selfWrapper,ref,RequestBlock(parentId,job._1,selfWrapper))
                }
                if (getActiveSlots(tine) == previousLen) {counter+=1} else {counter=0}
                foundAncestor = false
            }
          case None =>
            if (getActiveSlots(tine) == previousLen) {counter+=1} else {counter=0}
            foundAncestor = false
        }
      }
    }

    if (foundAncestor) {
      if (tine.largestGap < slotWindow) {
        if (holderIndex == SharedData.printingHolder && printFlag)
          println(s"Tine length = ${tine.length} Common prefix slot = $prefix")
        tinePoolWithPrefix = Array((tine,prefix,job._1)) ++ tinePoolWithPrefix
      }
      tinePool -= job._1
    } else {
      totalTries += 1
      tinePool -= job._1
      if (counter<tineMaxTries) tinePool += (job._1 -> (tine,counter,tine.length,totalTries,ref))
    }
  }

  def updateTine(inputTine:Tine): Option[(Tine,Slot)] = {
    val headIdOpt:Option[SlotId] = Try{inputTine.last}.toOption
    headIdOpt match {
      case Some(headId:SlotId) =>
        if (headId == localChain.get(headId._1)) {
          None
        } else {
          var prefix = -1
          var tine:Tine = Tine(subChain(inputTine,headId._1,headId._1))
          @scala.annotation.tailrec
          def loop(id:SlotId):Unit = {
            getParentId(id) match {
              case Some(pid:SlotId) =>
                if (pid == localChain.get(pid._1)) {
                  prefix = pid._1
                } else {
                  tine = tine ++ Tine(subChain(inputTine,pid._1,pid._1))
                  loop(pid)
                }
              case None =>
                println("Error: tineUpdate found no common prefix")
            }
          }
          loop(headId)
          Some((tine,prefix))
        }
      case None =>
        println("Error: invalid head id in updateTine")
        None
    }
  }

  /**main chain selection routine, maxvalid-bg*/
  def maxValidBG(): Unit = Try{
    val prefix:Slot = tinePoolWithPrefix.last._2
    val tine:Tine = Tine(tinePoolWithPrefix.last._1)
    val job:Int = tinePoolWithPrefix.last._3
    val tineMaxSlot = tine.last._1
    val bnt = getBlockHeader(tine.getLastActiveSlot(globalSlot)).get._9
    val bnl = getBlockHeader(localChain.getLastActiveSlot(globalSlot)).get._9

    if (holderIndex == SharedData.printingHolder) {
      val headId = localChain.getLastActiveSlot(globalSlot)
      val head = getBlockHeader(headId)
      println("Previous head: " + s" block ${head.get._9} "
        + Base58.encode(headId._2.data))
    }

    assert(!tine.isEmpty)

    if (job == bootStrapJob && bootStrapJob >= 0) {
      routerRef ! BootstrapJob(selfWrapper)
    }

    val bestChain = if(tineMaxSlot - prefix < k_s && bnl < bnt) {
      true
    } else {
      val slotsTine = getActiveSlots(subChain(tine,prefix+1,prefix+1+slotWindow))
      val slotsLocal = getActiveSlots(subChain(localChain,prefix+1,prefix+1+slotWindow))
      slotsLocal < slotsTine
    }

    if (bestChain) {
      if (verifySubChain(tine,localChain.lastActiveSlot(prefix))) {
        adoptTine()
        chainStorage.store(localChain,localChainId,serializer)
      } else {
        println("Error: invalid best chain")
        tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
        SharedData.throwError(holderIndex)
      }
    } else {
      dropTine()
    }

    def adoptTine():Unit = {
      if (holderIndex == SharedData.printingHolder && printFlag)
        println(s"Tine Adopted  $bnt  >  $bnl")
      collectLedger(subChain(localChain,prefix+1,globalSlot))
      collectLedger(tine)
      for (id <- subChain(localChain,prefix+1,globalSlot).ordered) {
        val ledger:TransactionSet = blocks.get(id).get.blockBody.get
        wallet.add(ledger)
      }
      for (i <- prefix+1 to globalSlot) {
        localChain.remove(i)
        val id = tine.get(i)
        if (id._1 > -1) {
          assert(id._1 == i)
          assert(
            getParentId(id) match {
              case Some(pid:SlotId) =>
                localChain.getLastActiveSlot(i) == pid
              case _ => false
            }
          )
          localChain.update(id,getNonce(id).get)
          val blockLedger:TransactionSet = blocks.get(id).get.blockBody.get
          for (trans<-blockLedger) {
            if (memPool.keySet.contains(trans.sid)) {
              memPool -= trans.sid
            }
          }
        }
      }
      val lastSlot = localChain.lastActiveSlot(globalSlot)
      history.get(localChain.get(lastSlot)) match {
        case Some(reorgState:(State,Eta)) =>
          localState = reorgState._1
          eta = reorgState._2
        case _ =>
          println("Error: invalid state and eta on adopted tine")
          SharedData.throwError(holderIndex)
      }
      var epoch = lastSlot / epochLength
      for (slot <- lastSlot to globalSlot) {
        updateEpoch(slot,epoch,eta,localChain) match {
          case result:(Int,Eta) if result._1 > epoch =>
            epoch = result._1
            eta = result._2
            stakingState = getStakingState(epoch,localChain)
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
            keys.alpha = alphaCache.get.get(keys.pkw)
          case _ =>
        }
      }
      assert(currentEpoch == epoch)
      updateWallet()
      trimMemPool()
      tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
      var newCandidateTines:Array[(Tine,Slot,Int)] = Array()
      for (entry <- tinePoolWithPrefix) {
        updateTine(entry._1) match {
          case Some((newTine:Tine,prefix:Slot)) =>
            if (prefix > 0 && !newTine.isEmpty) {
              newCandidateTines = newCandidateTines ++ Array((newTine,prefix,entry._3))
            }
          case None =>
        }
      }
      tinePoolWithPrefix = newCandidateTines
    }

    def dropTine():Unit = {
      if (holderIndex == SharedData.printingHolder && printFlag)
        println(s"Tine Rejected $bnt  <= $bnl")
      collectLedger(tine)
      for (id <- subChain(localChain,prefix+1,globalSlot).ordered) {
        if (id._1 > -1) {
          val blockLedger:TransactionSet = blocks.get(id).get.blockBody.get
          for (trans <- blockLedger) {
            if (memPool.keySet.contains(trans.sid)){
              memPool -= trans.sid
            }
          }
        }
      }
      tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
    }

    if (job == bootStrapJob && job >= 0 && !helloLock) {
      bootStrapJob = -1
      bootStrapLock = false
      routerRef ! BootstrapJob(selfWrapper)
    }

    if (holderIndex == SharedData.printingHolder) {
      val headId = localChain.getLastActiveSlot(globalSlot)
      val head = getBlockHeader(headId)
      println(Console.CYAN + "Current Slot = " + globalSlot.toString + s" on block ${head.get._9} "
        + Base58.encode(headId._2.data) + Console.RESET)
    }
    if (helloLock) {
      bootStrapMessage match {
        case scheduledMessage:Cancellable => scheduledMessage.cancel
        case null =>
      }
      bootStrapMessage = context.system.scheduler
        .scheduleOnce(2*slotT.millis,self,BootstrapJob)(context.system.dispatcher,self)
    }
  }

  def validateChainIds(c:Tine):Boolean = {
    var pid = c.least
    var out = true
    for (id <- c.ordered.tail) {
      getParentId(id) match {
        case Some(bid:SlotId) =>
          if (bid == pid) {
            if (history.known(id)) {
              pid = id
            } else {
              println(s"Holder $holderIndex error: could not find id in history")
              SharedData.throwError(holderIndex)
              out = false
            }
          } else {
            println(s"Holder $holderIndex error: pid mismatch in tine")
            SharedData.throwError(holderIndex)
            out = false
          }
        case _ =>
          println(s"Holder $holderIndex error: couldn't find parent in tine")
          SharedData.throwError(holderIndex)
          out = false
      }
    }
    out
  }
}
