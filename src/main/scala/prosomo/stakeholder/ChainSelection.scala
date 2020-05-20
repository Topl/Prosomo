package prosomo.stakeholder

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.{RequestBlock, RequestTine, SendTx}
import prosomo.components.{Block, Tine, Transaction}
import prosomo.primitives.{Parameters, Ratio, SharedData}

import scala.util.Try
import scala.util.control.Breaks.{break, breakable}

trait ChainSelection extends Members {
  import Parameters._

  def updateWallet = Try{
    var id = localChain.getLastActiveSlot(globalSlot)
    val bn:Int = getBlockHeader(id).get._9
    if (bn == 0) {
      wallet.update(history.get(id).get._1)
    } else {
      breakable{
        while (true) {
          id = getParentId(id).get
          getBlockHeader(id) match {
            case Some(b:BlockHeader) => {
              val bni = b._9
              if (bni <= bn-confirmationDepth || bni == 0) {
                wallet.update(history.get(id).get._1)
                break
              }
            }
            case None => {
              println("Error: invalid id in wallet")
              break
            }
          }
        }
      }
    }
    for (trans:Transaction <- wallet.getPending(localState)) {
      if (!memPool.keySet.contains(trans.sid)) memPool += (trans.sid->(trans,0))
      send(ActorRefWrapper(self),gossipers, SendTx(trans))
    }
    walletStorage.store(wallet,serializer)
  }

  def buildTine(job:(Int,(Tine,Int,Int,Int,ActorRefWrapper))): Unit = {
    val entry = job._2
    var foundAncestor = true
    var tine:Tine = Tine(entry._1)
    var counter:Int = entry._2
    val previousLen:Int = entry._3
    val totalTries:Int = entry._4
    val ref:ActorRefWrapper = entry._5
    var prefix:Slot = 0
    breakable{
      while(foundAncestor) {
        getParentId(tine.least) match {
          case Some(parentId:SlotId) => {
            getBlockHeader(parentId) match {
              case Some(pbh:BlockHeader) => {
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
              }
              case None => {
                if (counter>2*tineMaxTries) {
                  if (holderIndex == SharedData.printingHolder && printFlag) println("Holder " + holderIndex.toString + " Dropping Old Tine")
                  tines -= job._1
                } else {
                  tines -= job._1
                  val tineLength = getActiveSlots(tine)
                  tines += (job._1 -> (tine,counter,tineLength,totalTries+1,ref))
                  if (totalTries > tineMaxTries || tineLength>tineMaxDepth) {
                    bootStrapLock = true
                    bootStrapJob = job._1
                    if (holderIndex == SharedData.printingHolder && printFlag) println(
                      "Holder " + holderIndex.toString + " Looking for Parent Tine, Job:"+job._1+" Tries:"+counter.toString+" Length:"+tineLength+" Tines:"+tines.keySet.size
                    )
                    val depth:Int = if (tineLength < tineMaxDepth) {
                      tineLength
                    } else {
                      tineMaxDepth
                    }
                    val request:Request = (List(parentId),depth,job._1)
                    send(ActorRefWrapper(self),ref, RequestTine(parentId,depth,signMac(hash(request,serializer),sessionId,keys.sk_sig,keys.pk_sig),job._1))
                  } else {
                    if (holderIndex == SharedData.printingHolder && printFlag) println(
                      "Holder " + holderIndex.toString + " Looking for Parent Block, Job:"+job._1+" Tries:"+counter.toString+" Length:"+getActiveSlots(tine)+" Tines:"+tines.keySet.size
                    )
                    val request:Request = (List(parentId),0,job._1)
                    send(ActorRefWrapper(self),ref, RequestBlock(parentId,signMac(hash(request,serializer),sessionId,keys.sk_sig,keys.pk_sig),job._1))
                  }
                }
                if (getActiveSlots(tine) == previousLen) {counter+=1} else {counter=0}
                foundAncestor = false
              }
            }
          }
          case None => {
            if (getActiveSlots(tine) == previousLen) {counter+=1} else {counter=0}
            foundAncestor = false
          }
        }
      }
    }

    if (foundAncestor) {
      candidateTines = Array((tine,prefix,job._1)) ++ candidateTines
      tines -= job._1
    }
  }

  def updateTine(inputTine:Tine): Option[(Tine,Slot)] = {
    val headIdOpt:Option[SlotId] = Try{inputTine.last}.toOption
    headIdOpt match {
      case Some(headId:SlotId) => {
        if (headId == localChain.get(headId._1)) {
          None
        } else {
          var prefix = -1
          var tine:Tine = Tine(subChain(inputTine,headId._1,headId._1))
          def loop(id:SlotId):Unit = {
            getParentId(id) match {
              case Some(pid:SlotId) => {
                if (pid == localChain.get(pid._1)) {
                  prefix = pid._1
                } else {
                  tine = tine ++ Tine(subChain(inputTine,pid._1,pid._1))
                  loop(pid)
                }
              }
              case None => {
                println("Error: tineUpdate found no common prefix")
              }
            }
          }
          loop(headId)
          Some((tine,prefix))
        }
      }
      case None => {
        println("Error: invalid head id in updateTine")
        None
      }
    }
  }

  /**main chain selection routine, maxvalid-bg*/
  def maxValidBG = Try{
    val prefix:Slot = candidateTines.last._2
    val tine:Tine = Tine(candidateTines.last._1)
    val job:Int = candidateTines.last._3
    val tineMaxSlot = tine.last._1
    val bnt = getBlockHeader(tine.getLastActiveSlot(globalSlot)).get._9
    val bnl = getBlockHeader(localChain.getLastActiveSlot(globalSlot)).get._9

    assert(!tine.isEmpty)

    if (job == bootStrapJob) {
      bootStrapJob = -1
      bootStrapLock = false
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
        adoptTine
        chainStorage.store(localChain,localChainId,serializer)
      } else {
        println("Error: invalid best chain")
        candidateTines = candidateTines.dropRight(1)
        SharedData.throwError(holderIndex)
      }
    } else {
      dropTine
    }

    def adoptTine:Unit = {
      if (holderIndex == SharedData.printingHolder && printFlag)
        println("Holder " + holderIndex.toString + " Adopting Chain")
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
              case Some(pid:SlotId) => {
                localChain.getLastActiveSlot(i) == pid
              }
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
        case Some(reorgState:(State,Eta)) => {
          localState = reorgState._1
          eta = reorgState._2
          //println(s"Holder $holderIndex set eta to "+Base58.encode(eta))
        }
        case _ => {
          println("Error: invalid state and eta on adopted tine")
          SharedData.throwError(holderIndex)
        }
      }
      var epoch = lastSlot / epochLength
      for (slot <- lastSlot to globalSlot) {
        updateEpoch(slot,epoch,eta,localChain) match {
          case result:(Int,Eta) if result._1 > epoch => {
            epoch = result._1
            eta = result._2
            stakingState = getStakingState(epoch,localChain)
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
            keys.alpha = alphaCache.get.get(keys.pkw)
          }
          case _ =>
        }
      }
      assert(currentEpoch == epoch)
      updateWallet
      trimMemPool
      candidateTines = candidateTines.dropRight(1)
      var newCandidateTines:Array[(Tine,Slot,Int)] = Array()
      for (entry <- candidateTines) {
        updateTine(entry._1) match {
          case Some((newTine:Tine,prefix:Slot)) => {
            if (prefix > 0 && !newTine.isEmpty) {
              newCandidateTines = newCandidateTines ++ Array((newTine,prefix,entry._3))
            }
          }
          case None =>
        }
      }
      candidateTines = newCandidateTines
    }

    def dropTine:Unit = {
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
      candidateTines = candidateTines.dropRight(1)
    }


  }

  def validateChainIds(c:Tine):Boolean = {
    var pid = c.least
    var out = true
    for (id <- c.ordered.tail) {
      getParentId(id) match {
        case Some(bid:SlotId) => {
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
        }
        case _ => {
          println(s"Holder $holderIndex error: couldn't find parent in tine")
          SharedData.throwError(holderIndex)
          out = false
        }
      }
    }
    out
  }

}
