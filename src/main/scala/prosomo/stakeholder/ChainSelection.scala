package prosomo.stakeholder

import akka.actor.ActorRef
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.{RequestBlock, RequestChain, SendTx}
import prosomo.components.{Chain, Transaction}
import prosomo.primitives.{Parameters, SharedData}
import scorex.crypto.encode.Base58

import scala.util.control.Breaks.{break, breakable}

trait ChainSelection extends Members {
  import Parameters._

  def updateTine(inputTine:Chain): (Chain,Slot) = {
    var foundAncestor = true
    var tine:Chain = Chain(inputTine)
    var prefix:Slot = 0
    if (localChain.get(tine.head._1) == tine.head) {
      (tine,-1)
    } else {
      breakable{
        while(foundAncestor) {
          getParentId(tine.head) match {
            case pb:SlotId => {
              tine = Chain(pb) ++ tine
              if (tine.head == localChain.get(tine.head._1)) {
                prefix = tine.head._1
                tine = Chain(tine.ordered.tail)
                break
              }
              if (tine.head._1 == 0) {
                prefix = 0
                tine = Chain(tine.ordered.tail)
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
          history.get(id._2,serializer) match {
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
                history.get(id._2,serializer) match {
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
      send(self,gossipers, SendTx(trans))
    }
    walletStorage.store(wallet,serializer)
  }

  def buildTine(job:(Int,(Chain,Int,Int,Int,ActorRef))): Unit = {
    val entry = job._2
    var foundAncestor = true
    var tine:Chain = Chain(entry._1)
    var counter:Int = entry._2
    val previousLen:Int = entry._3
    val totalTries:Int = entry._4
    val ref:ActorRef = entry._5
    var prefix:Slot = 0
    breakable{
      while(foundAncestor) {
        getParentId(tine.head) match {
          case pb:SlotId => {
            tine = Chain(pb) ++ tine
            if (tine.head == localChain.get(tine.head._1)) {
              prefix = tine.head._1
              tine = Chain(tine.ordered.tail)
              break
            }
            if (tine.head._1 == 0) {
              prefix = 0
              tine = Chain(tine.ordered.tail)
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
        tines += (job._1 -> (tine,counter,getActiveSlots(tine),totalTries+1,ref))
        if (totalTries > tineMaxTries) {
          if (holderIndex == SharedData.printingHolder && printFlag) println(
            "Holder " + holderIndex.toString + " Looking for Parent Blocks C:"+counter.toString+"L:"+getActiveSlots(tine)
          )
          val depth:Int = if (totalTries - tineMaxTries < tineMaxDepth) {
            totalTries - tineMaxTries
          } else {
            tineMaxDepth
          }
          val request:Request = (List(tine.head._2),depth,job._1)
          send(self,ref, RequestChain(tine.head._2,depth,signMac(hash(request,serializer),sessionId,keys.sk_sig,keys.pk_sig),job._1))
        } else {
          if (holderIndex == SharedData.printingHolder && printFlag) println("Holder " + holderIndex.toString + " Looking for Parent Block C:"+counter.toString+"L:"+getActiveSlots(tine))
          val request:Request = (List(tine.head._2),0,job._1)
          send(self,ref, RequestBlock(tine.head._2,signMac(hash(request,serializer),sessionId,keys.sk_sig,keys.pk_sig),job._1))
        }
      }
    }
  }

  /**main chain selection routine, maxvalid-bg*/
  def maxValidBG = {
    val prefix:Slot = candidateTines.last._2
    val tine:Chain = Chain(candidateTines.last._1)
    val job:Int = candidateTines.last._3
    val tineMaxSlot = tine.last._1
    val bnt = {getBlockHeader(tine.getLastActiveSlot(globalSlot)) match {case b:BlockHeader => b._9}}
    val bnl = {getBlockHeader(localChain.getLastActiveSlot(globalSlot)) match {case b:BlockHeader => b._9}}

    def adoptTine:Unit = {
      if (holderIndex == SharedData.printingHolder && printFlag)
        println("Holder " + holderIndex.toString + " Adopting Chain")
      collectLedger(subChain(localChain,prefix+1,globalSlot))
      collectLedger(tine)
      for (id <- subChain(localChain,prefix+1,globalSlot).ordered) {
        val ledger:TransactionSet = blocks.getTxs(id,serializer)
        wallet.add(ledger)
      }
      for (i <- prefix+1 to globalSlot) {
        localChain.remove(i)
        val id = tine.get(i)
        if (id._1 > -1) {
          assert(id._1 == i)
          chainHistory.update(id,serializer)
          assert(
            getParentId(id) match {
              case pid:SlotId => {
                localChain.getLastActiveSlot(i) == pid
              }
              case _ => false
            }
          )
          localChain.update(id)
          val blockLedger:TransactionSet = blocks.getTxs(id,serializer)
          for (trans<-blockLedger) {
            if (memPool.keySet.contains(trans.sid)) {
              memPool -= trans.sid
            }
          }
        } else {
          chainHistory.update((-1,ByteArrayWrapper(Array())),serializer)
        }
      }
      candidateTines = candidateTines.dropRight(1)
      var newCandidateTines:Array[(Chain,Slot,Int)] = Array()
      for (entry <- candidateTines) {
        val newTine = updateTine(Chain(entry._1.last))
        if (newTine._2 > 0) {
          newCandidateTines = newCandidateTines ++ Array((newTine._1,newTine._2,entry._3))
        }
      }
      candidateTines = newCandidateTines
      updateWallet
      trimMemPool
      history.get(localChain.getLastActiveSlot(globalSlot)._2,serializer) match {
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
      var epoch = localChain.lastActiveSlot(globalSlot) / epochLength
      updateStakingState(epoch)
      for (slot <- localChain.lastActiveSlot(globalSlot) to globalSlot) {
        updateEpoch(slot,epoch) match {
          case ep:Int if ep > epoch => epoch = ep
          case _ =>
        }
      }
    }

    def dropTine:Unit = {
      collectLedger(tine)
      for (id <- subChain(localChain,prefix+1,globalSlot).ordered) {
        if (id._1 > -1) {
          val blockLedger:TransactionSet = blocks.getTxs(id,serializer)
          for (trans <- blockLedger) {
            if (memPool.keySet.contains(trans.sid)){
              memPool -= trans.sid
            }
          }
        }
      }
      candidateTines = candidateTines.dropRight(1)
    }

    var trueChain = false

    if(tineMaxSlot - prefix < k_s && bnl < bnt) {
      trueChain = true
    } else {
      val slotsTine = getActiveSlots(subChain(tine,prefix+1,prefix+1+slotWindow))
      val slotsLocal = getActiveSlots(subChain(localChain,prefix+1,prefix+1+slotWindow))
      if (slotsLocal < slotsTine) {
        trueChain = true
      }
    }

    if (trueChain) {
      trueChain &&= verifySubChain(tine,prefix)
    }

    if (trueChain) {
      adoptTine
    } else {
      dropTine
    }
    assert(validateChainIds(localChain))
    chainStorage.store(localChain,localChainId,serializer)
  }

  def validateChainIds(c:Chain):Boolean = {
    var pid = c.head
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
