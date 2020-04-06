package prosomo.stakeholder

import prosomo.cases.{RequestBlock, RequestTine, SendTx}
import prosomo.components.{Tine, Transaction}
import prosomo.primitives.{Parameters, SharedData}

import scala.util.control.Breaks.{break, breakable}

trait ChainSelection extends Members {
  import Parameters._

  def updateTine(inputTine:Tine): (Tine,Slot) = {
    var foundAncestor = true
    var tine:Tine = Tine(inputTine)
    var prefix:Slot = 0
    if (localChain.get(tine.least._1) == tine.least) {
      (tine,-1)
    } else {
      breakable{
        while(foundAncestor) {
          getParentId(tine.least) match {
            case pb:SlotId => {
              tine = Tine(pb,getNonce(pb)) ++ tine
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
            case _ => {
              foundAncestor = false
              println("Error: update tine found no common prefix")
              prefix = -1
            }
          }
        }
      }
      (tine,prefix)
    }
  }

  def updateWallet = {
    var id = localChain.getLastActiveSlot(globalSlot)
    val bn = {getBlockHeader(id) match {case b:BlockHeader => b._9}}
    if (bn == 0) {
      getBlockHeader(id) match {
        case b:BlockHeader => {
          val bni = b._9
          history.get(id) match {
            case value:(State,Eta) => {
              wallet.update(value._1)
            }
          }
        }
      }
    } else {
      breakable{
        while (true) {
          id = getParentId(id) match {case value:SlotId => value}
          getBlockHeader(id) match {
            case b:BlockHeader => {
              val bni = b._9
              if (bni <= bn-confirmationDepth || bni == 0) {
                history.get(id) match {
                  case value:(State,Eta) => {
                    wallet.update(value._1)
                  }
                }
                break
              }
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
          case pb:SlotId => {
            tine = Tine(pb,getNonce(pb)) ++ tine
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
          case _ => {
            if (getActiveSlots(tine) == previousLen) {counter+=1} else {counter=0}
            foundAncestor = false
          }
        }
      }
    }
    if (foundAncestor) {
      candidateTines = Array((tine,prefix,job._1)) ++ candidateTines
      tines -= job._1
    } else {
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
          val request:Request = (List(tine.least),depth,job._1)
          send(ActorRefWrapper(self),ref, RequestTine(tine.least,depth,signMac(hash(request,serializer),sessionId,keys.sk_sig,keys.pk_sig),job._1))
        } else {
          if (holderIndex == SharedData.printingHolder && printFlag) println(
            "Holder " + holderIndex.toString + " Looking for Parent Block, Job:"+job._1+" Tries:"+counter.toString+" Length:"+getActiveSlots(tine)+" Tines:"+tines.keySet.size
          )
          val request:Request = (List(tine.least),0,job._1)
          send(ActorRefWrapper(self),ref, RequestBlock(tine.least,signMac(hash(request,serializer),sessionId,keys.sk_sig,keys.pk_sig),job._1))
        }
      }
    }
  }

  /**main chain selection routine, maxvalid-bg*/
  def maxValidBG = {
    val prefix:Slot = candidateTines.last._2
    val tine:Tine = Tine(candidateTines.last._1)
    val job:Int = candidateTines.last._3
    val tineMaxSlot = tine.last._1
    val bnt = {getBlockHeader(tine.getLastActiveSlot(globalSlot)) match {case b:BlockHeader => b._9}}
    val bnl = {getBlockHeader(localChain.getLastActiveSlot(globalSlot)) match {case b:BlockHeader => b._9}}

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
        println("error: invalid best chain")
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
        val ledger:TransactionSet = blocks.getTxs(id)
        wallet.add(ledger)
      }
      for (i <- prefix+1 to globalSlot) {
        localChain.remove(i)
        val id = tine.get(i)
        if (id._1 > -1) {
          assert(id._1 == i)
          //chainHistory.update(id,serializer)
          assert(
            getParentId(id) match {
              case pid:SlotId => {
                localChain.getLastActiveSlot(i) == pid
              }
              case _ => false
            }
          )
          localChain.update(id,getNonce(id))
          val blockLedger:TransactionSet = blocks.getTxs(id)
          for (trans<-blockLedger) {
            if (memPool.keySet.contains(trans.sid)) {
              memPool -= trans.sid
            }
          }
        } else {
          //chainHistory.update((-1,ByteArrayWrapper(Array())),serializer)
        }
      }
      val lastSlot = localChain.lastActiveSlot(globalSlot)
      history.get(localChain.get(lastSlot)) match {
        case reorgState:(State,Eta) => {
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
            keys.alpha = relativeStake(keys.pkw,stakingState)
            keys.threshold = phi(keys.alpha)
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
        val bid:SlotId = entry._1.last
        val newTine = updateTine(Tine(bid,getNonce(bid)))
        if (newTine._2 > 0) {
          newCandidateTines = newCandidateTines ++ Array((newTine._1,newTine._2,entry._3))
        }
      }
      candidateTines = newCandidateTines
    }

    def dropTine:Unit = {
      collectLedger(tine)
      for (id <- subChain(localChain,prefix+1,globalSlot).ordered) {
        if (id._1 > -1) {
          val blockLedger:TransactionSet = blocks.getTxs(id)
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
        case bid:SlotId => {
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
