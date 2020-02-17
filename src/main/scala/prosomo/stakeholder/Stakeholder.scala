package prosomo.stakeholder

import java.io.BufferedWriter

import akka.actor.{Actor, ActorPath, ActorRef, Props, Timers}
import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases._
import prosomo.primitives.{Kes, Keys, SharedData, Sig, Vrf, Ratio, Parameters}
import prosomo.components.{Block, BlockData, Box, Chain, Methods, Serializer, SlotReorgHistory, Transaction}
import prosomo.history.History
import prosomo.wallet.Wallet
import scorex.crypto.encode.Base58

import scala.math.BigInt
import scala.util.Random
import scala.util.control.Breaks._

/**
  * Stakeholder actor that executes the staking protocol and communicates with other stakeholders,
  * sends the coordinator the public key upon instantiation and gets the genesis block from coordinator
  */

class Stakeholder(seed:Array[Byte]) extends Actor
  with Timers
  with Methods {
  import Parameters._
  val serializer:Serializer = new Serializer
  //vars for chain, blocks, state, history, and locks
  val storageDir:String = dataFileDir+"/"+self.path.toStringWithoutAddress.drop(5)
  var localChain:Chain = _
  var blocks:BlockData = new BlockData(storageDir)
  var chainHistory:SlotReorgHistory = new SlotReorgHistory
  var localState:State = Map()
  var eta:Eta = Array()
  var stakingState:State = Map()
  var memPool:MemPool = Map()
  var holderIndex:Int = -1
  var diffuseSent = false

  //verification and signing objects
  val vrf = new Vrf
  val kes = new Kes
  val sig = new Sig

  val history:History = new History
  //val mempool:Mempool = new Mempool
  var rng:Random = new Random
  var routerRef:ActorRef = _

  val holderId:ActorPath = self.path
  val sessionId:Sid = ByteArrayWrapper(FastCryptographicHash(holderId.toString))
  rng = new Random(BigInt(seed).toLong)
  val phase:Double = rng.nextDouble
  var chainUpdateLock = false
  val keys:Keys = Keys(seed,sig,vrf,kes,0)
  val wallet:Wallet = new Wallet(keys.pkw,fee_r)

  //list of all or some of the stakeholders, including self, that the stakeholder is aware of
  var holders: List[ActorRef] = List()
  //list of stakeholders that all new blocks and transactions are sent to
  var gossipers: List[ActorRef] = List()
  //gossipers offset
  var gOff = 0
  //number of tries to issue hello in slots
  var numHello = 0
  //map of all session IDs and public keys associated with holders in holder list
  var inbox:Map[Sid,(ActorRef,PublicKeys)] = Map()
  //total number of times this stakeholder was elected slot leader
  var blocksForged = 0
  //slot time as determined from coordinator clock
  var globalSlot = 0
  //all tines that are pending built from new blocks that are received
  var tines:Map[Int,(Chain,Int,Int,Int,ActorRef)] = Map()
  //counter for identifying tines
  var tineCounter = 0
  //completed tines waiting to be selected with maxvalid-bg
  var candidateTines:Array[(Chain,Slot,Int)] = Array()
  //placeholder for genesis block
  var genBlockHeader: BlockHeader = _
  //placeholder for genesis block ID
  var genBlockHash: Hash = ByteArrayWrapper(Array())
  //placeholder for forged block if elected slot leader
  var roundBlock: Any = 0
  //max time steps set by coordinator
  var tMax = 0
  //start system time set by coordinator
  var t0:Long = 0
  //current slot that is being processed by stakeholder
  var localSlot = 0
  //current epoch that is being processed by stakeholder
  var currentEpoch = -1
  //lock for update message
  var updating = false
  //lock for stalling stakeholder
  var actorStalled = false
  //ref of coordinator actor
  var coordinatorRef:ActorRef = _
  //total number of transactions issued
  var txCounter = 0
  //set of all txs issued by holder
  var setOfTxs:Map[Sid,Int] = Map()
  //toggle if holder is adversary
  var adversary:Boolean = false
  //toggle for covert mining
  var covert:Boolean = false
  //toggle for nothing-at-stake forging
  var forgeAll:Boolean = false

  private case object timerKey

  def blockInfo:String = {
    "forger_index:"+holderIndex.toString+",adversarial:"+adversary.toString+",eta:"+Base58.encode(eta)+",epoch:"+currentEpoch.toString
  }

  /**determines eligibility for a stakeholder to be a slot leader then calculates a block with epoch variables */
  def forgeBlock(forgerKeys:Keys) = {
    val slot = localSlot
    val pi_y: Pi = vrf.vrfProof(forgerKeys.sk_vrf, eta ++ serializer.getBytes(slot) ++ serializer.getBytes("TEST"))
    val y: Rho = vrf.vrfProofToHash(pi_y)
    if (compare(y, forgerKeys.threshold)) {
      //println("Eta on forging:"+Base58.encode(eta))
      roundBlock = {
        val pb:BlockHeader = getBlockHeader(localChain.getLastActiveSlot(localSlot-1)) match {case b:BlockHeader => b}
        val bn:Int = pb._9 + 1
        val ps:Slot = pb._3
        val txs:TransactionSet = chooseLedger(forgerKeys.pkw,memPool,localState)
        val pi: Pi = vrf.vrfProof(forgerKeys.sk_vrf, eta ++ serializer.getBytes(slot) ++ serializer.getBytes("NONCE"))
        val rho: Rho = vrf.vrfProofToHash(pi)
        val h: Hash = hash(pb,serializer)
        val ledger:Box = signBox(hash(txs,serializer), sessionId, forgerKeys.sk_sig, forgerKeys.pk_sig)
        val cert: Cert = (forgerKeys.pk_vrf, y, pi_y, forgerKeys.pk_sig, forgerKeys.threshold,blockInfo)
        val kes_sig: KesSignature = forgerKeys.sk_kes.sign(kes,h.data++serializer.getBytes(ledger)++serializer.getBytes(slot)++serializer.getBytes(cert)++rho++pi++serializer.getBytes(bn)++serializer.getBytes(ps))
        val b = (h, ledger, slot, cert, rho, pi, kes_sig, forgerKeys.pk_kes,bn,ps)
        val hb = hash(b,serializer)
        if (printFlag) {println("Holder " + holderIndex.toString + s" forged block $bn with id:"+Base58.encode(hb.data))}
        val block = new Block(hb,b,txs)
        blocks.add(block)
        assert(localChain.getLastActiveSlot(localSlot)._2 == b._1)
        localChain.update((localSlot, hb))
        chainHistory.update((localSlot,hb))
        send(self,gossipers, SendBlock(block,signBox(block.id, sessionId, keys.sk_sig, keys.pk_sig)))
        blocksForged += 1
        updateLocalState(localState, Chain(localChain.get(localSlot))) match {
          case value:State => localState = value
          case _ => {
            SharedData.throwError(holderIndex)
            println("error: invalid ledger in forged block")
          }
        }
        history.add(hb,localState,eta)
        updateWallet
        trimMemPool
        validateChainIds(localChain)
        b
      }
    } else {
      roundBlock = -1
    }
  }

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
    var id = localChain.getLastActiveSlot(localSlot)
    val bn = {getBlockHeader(id) match {case b:BlockHeader => b._9}}
    if (bn == 0) {
      getBlockHeader(id) match {
        case b:BlockHeader => {
          val bni = b._9
          history.get(id._2) match {
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
                history.get(id._2) match {
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
      send(self,gossipers, SendTx(trans,signBox(hash(trans,serializer),sessionId,keys.sk_sig,keys.pk_sig)))
    }
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
          send(self,ref, RequestChain(tine.head._2,depth,signBox(hash(request,serializer),sessionId,keys.sk_sig,keys.pk_sig),job._1))
        } else {
          if (holderIndex == SharedData.printingHolder && printFlag) println("Holder " + holderIndex.toString + " Looking for Parent Block C:"+counter.toString+"L:"+getActiveSlots(tine))
          val request:Request = (List(tine.head._2),0,job._1)
          send(self,ref, RequestBlock(tine.head._2,signBox(hash(request,serializer),sessionId,keys.sk_sig,keys.pk_sig),job._1))
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
    val bnt = {getBlockHeader(tine.getLastActiveSlot(localSlot)) match {case b:BlockHeader => b._9}}
    val bnl = {getBlockHeader(localChain.getLastActiveSlot(localSlot)) match {case b:BlockHeader => b._9}}

    def adoptTine:Unit = {
      if (holderIndex == SharedData.printingHolder && printFlag) println("Holder " + holderIndex.toString + " Adopting Chain")
      collectLedger(subChain(localChain,prefix+1,localSlot))
      collectLedger(tine)
      for (id <- subChain(localChain,prefix+1,localSlot).ordered) {
        val ledger:TransactionSet = blocks.getTxs(id)
        wallet.add(ledger)
      }
      for (i <- prefix+1 to localSlot) {
        localChain.remove(i)
        val id = tine.get(i)
        if (id._1 > -1) {
          assert(id._1 == i)
          chainHistory.update(id)
          assert(
            getParentId(id) match {
              case pid:SlotId => {
                localChain.getLastActiveSlot(i) == pid
              }
              case _ => false
            }
          )
          localChain.update(id)
          val blockLedger:TransactionSet = blocks.getTxs(id)
          for (trans<-blockLedger) {
            if (memPool.keySet.contains(trans.sid)) {
              memPool -= trans.sid
            }
          }
        } else {
          chainHistory.update((-1,ByteArrayWrapper(Array())))
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
      history.get(localChain.getLastActiveSlot(localSlot)._2) match {
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
      var epoch = localChain.lastActiveSlot(localSlot) / epochLength
      updateStakingState(epoch)
      for (slot <- localChain.lastActiveSlot(localSlot) to localSlot) {
        updateEpoch(slot,epoch) match {
          case ep:Int if ep > epoch => epoch = ep
          case _ =>
        }
      }
    }

    def dropTine:Unit = {
      collectLedger(tine)
      for (id <- subChain(localChain,prefix+1,localSlot).ordered) {
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

    validateChainIds(localChain)
  }

  def validateChainIds(c:Chain) = {
    var pid = c.head
    for (id <- c.ordered.tail) {
      getParentId(id) match {
        case bid:SlotId => {
          if (bid == pid) {
            if (history.known(id)) {
              pid = id
            } else {
              println(s"Holder $holderIndex error: could not find id in history")
            }
          } else {
            println(s"Holder $holderIndex error: pid mismatch in tine")
            SharedData.throwError(holderIndex)
          }
        }
        case _ => {
          println(s"Holder $holderIndex error: couldn't find parent in tine")
          SharedData.throwError(holderIndex)
        }
      }
    }
  }

  /**slot routine, called every time currentSlot increments*/
  def updateSlot = {
    updateEpoch(localSlot,currentEpoch) match {
      case ep:Int if ep > currentEpoch => currentEpoch = ep
      case _ =>
    }
    if (localSlot == globalSlot) {
      timeFlag(
        if (keys.sk_kes.time(kes) < localSlot) {
          if (holderIndex == SharedData.printingHolder && printFlag && localSlot%epochLength == 0) {
            println("Current Epoch = " + currentEpoch.toString)
            println("Holder " + holderIndex.toString + " alpha = " + keys.alpha.toDoubleString+"\nEta:"+Base58.encode(eta))
          }
          roundBlock = 0
          if (holderIndex == SharedData.printingHolder) println(Console.CYAN + "Slot = " + localSlot.toString + " on block "
            + Base58.encode(localChain.getLastActiveSlot(globalSlot)._2.data) + Console.WHITE)
          keys.sk_kes.update(kes, localSlot)
          if (useGossipProtocol) {
            val newOff = (numGossipers*math.sin(2.0*math.Pi*(globalSlot.toDouble/100.0+phase))/2.0).toInt
            if (newOff != gOff) {
              if (gOff < newOff) numHello = 0
              gOff = newOff
            }
            if (gossipers.length < numGossipers + gOff && numHello < 1) {
              send(self,rng.shuffle(holders.filter(_!=self)),Hello(self,signBox(hash(self,serializer), sessionId, keys.sk_sig, keys.pk_sig)))
              numHello += 1
            } else if (gossipers.length > numGossipers + gOff) {
              gossipers = rng.shuffle(gossipers).take(numGossipers + gOff)
            }
          }
        }
      )
      updateLocalState(localState, Chain(localChain.get(localSlot))) match {
        case value:State => localState = value
        case _ => {
          SharedData.throwError(holderIndex)
          println("error: invalid block ledger on chain")
        }
      }
      if (dataOutFlag && globalSlot % dataOutInterval == 0) {
        coordinatorRef ! WriteFile
      }
    }
  }

  /**epoch routine, called every time currentEpoch increments*/
  def updateEpoch(slot:Slot,epochIn:Int):Int = {
    var ep = epochIn
    if (slot / epochLength > ep) {
      ep = slot / epochLength
      updateStakingState(ep)
      if (ep > 0) {
        eta = eta(localChain, ep, eta)
        //println(s"Holder $holderIndex set eta to "+Base58.encode(eta))
      }
    }
    ep
  }

  def updateStakingState(ep:Int):Unit = {
    stakingState = {
      if (ep > 1) {
        val eps:Slot = (ep-1)*epochLength
        history.get(localChain.getLastActiveSlot(eps)._2) match {
          case value:(State,Eta) => {
            value._1
          }
          case _ => {
            val thisSlot = lastActiveSlot(localChain,eps)
            println(s"Could not recover staking state ep $ep slot $thisSlot id:"+Base58.encode(localChain.getLastActiveSlot(eps)._2.data))
            localChain.print
            SharedData.throwError(holderIndex)
            Map()
          }
        }
      } else {
        history.get(localChain.get(0)._2) match {
          case value:(State,Eta) => {
            value._1
          }
          case _ => {
            println("Could not recover staking state ep 0")
            SharedData.throwError(holderIndex)
            Map()
          }
        }
      }
    }
    keys.alpha = relativeStake(keys.pkw, stakingState)
    keys.threshold = phi(keys.alpha)
  }

  def update = { if (SharedData.error) {actorStalled = true}
    if (!actorStalled) {
      if (!updating) {
        updating = true
        if (globalSlot > tMax || SharedData.killFlag) {
          timers.cancelAll
        } else if (diffuseSent) {
          if (!useFencing) coordinatorRef ! GetTime
          if (globalSlot > localSlot) {
            while (globalSlot > localSlot) {
              localSlot += 1
              updateSlot
            }
          } else if (roundBlock == 0 && candidateTines.isEmpty) {
            forgeBlock(keys)
            if (useFencing) {routerRef ! (self,"updateSlot")}
          } else if (!useFencing && candidateTines.nonEmpty) {
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Checking Tine")
            }
            timeFlag(maxValidBG)
          } else if (useFencing && chainUpdateLock) {
            if (candidateTines.isEmpty) {
              chainUpdateLock = false
            } else {
              if (holderIndex == SharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Checking Tine")
              }
              timeFlag(maxValidBG)
            }
          }
        }
        updating = false
      }
    }
  }


  def receive: Receive = {

/**************************************************** Holders *********************************************************/

      /**updates time, the kes key, and resets variables */
    case Update => {
      if (adversary) {
        update
      } else {
        update
      }
    }

    case value:GetSlot => {
      if (!actorStalled) {
        if (roundBlock == 0) globalSlot += 1
        assert(globalSlot == value.s)
        while (roundBlock == 0) {
          update
        }
      } else {
        if (useFencing) {routerRef ! (self,"updateSlot")}
      }
      sender() ! "done"
    }

    case "endStep" => if (useFencing) {
      roundBlock = 0
      routerRef ! (self,"endStep")
    }

    case "passData" => if (useFencing) {
      routerRef ! (self,"passData")
    }

    case value:Adversary => {
      value.s match {
        case "" => {
          if (adversary) {
            adversary=false
          } else {
            adversary=true
          }
        }
        case "covert" => {
          if (covert) {
            adversary=false
            covert=false
          } else {
            adversary=true
            covert=true
          }
        }
        case "nas" => {
          if (forgeAll) {
            adversary = false
            forgeAll = false
          } else {
            adversary = true
            forgeAll = true
          }
        }
        case _ => "error: Adversary command unknown"
      }
      sender() ! "done"
    }

      /**adds confirmed transactions to buffer and sends new ones to gossipers*/
    case value:SendTx => {
      if (!actorStalled) {
        if (!memPool.keySet.contains(value.transaction.sid) && localState.keySet.contains(value.transaction.sender)) {
          if (localState(value.transaction.sender)._3 <= value.transaction.nonce) {
            if (verifyTransaction(value.transaction)) {
              memPool += (value.transaction.sid->(value.transaction,0))
              send(self,gossipers, SendTx(value.transaction,signBox(hash(value.transaction,serializer),sessionId,keys.sk_sig,keys.pk_sig)))
            }
          }
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

      /**block passing, new blocks delivered are added to list of tines and then sent to gossipers*/
    case value:SendBlock => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.box.sid)) {
          val foundBlock = blocks.known(value.block.id)
          if (!foundBlock) {
            val b:BlockHeader = value.block.prosomoHeader
            val bHash = hash(b,serializer)
            val bSlot = b._3
            if (verifyBlock(value.block) && verifyBox(value.block.id,value.box)) {
              blocks.add(value.block)
              if (bSlot <= globalSlot) {
                if (holderIndex == SharedData.printingHolder && printFlag) {
                  println("Holder " + holderIndex.toString + " Got New Tine")
                }
                val newId = (bSlot, bHash)
                send(self,gossipers, SendBlock(value.block,signBox(value.block.id, sessionId, keys.sk_sig, keys.pk_sig)))
                val jobNumber = tineCounter
                tines += (jobNumber -> (Chain(newId),0,0,0,inbox(value.box.sid)._1))
                buildTine((jobNumber,tines(jobNumber)))
                tineCounter += 1
              }
            } else {
              println("error: invalid block info")
            }
          }
        }
      }
      if (useFencing) {
        chainUpdateLock = true
        while (chainUpdateLock) {
          update
        }
        routerRef ! (self,"passData")
      }
    }

      /**block passing, returned blocks are added to block database*/
    case value:ReturnBlock => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.box.sid)) {
          if (verifyBox(hash((value.blocks.map(_.id),0,value.job),serializer),value.box)) {
            for (block <- value.blocks) {
              if (!blocks.known(block.id)) {
                if (verifyBlock(block)) {
                  if (holderIndex == SharedData.printingHolder && printFlag) {
                    println("Holder " + holderIndex.toString + " Got Block "+Base58.encode(block.id.data))
                  }
                  blocks.add(block)
                } else {"error: invalid returned block"}
                if (tines.keySet.contains(value.job)) buildTine((value.job,tines(value.job)))
              }
            }
          } else {println("error: invalid block list")}
        }
      }
      if (useFencing) {
        chainUpdateLock = true
        while (chainUpdateLock) {
          update
        }
        routerRef ! (self,"passData")
      }
    }

      /**block passing, parent ids that are not found are requested*/
    case value:RequestBlock => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.box.sid)) {
          if (holderIndex == SharedData.printingHolder && printFlag) {
            println("Holder " + holderIndex.toString + " Was Requested Block")
          }
          if (verifyBox(hash((List(value.id),0,value.job),serializer),value.box)) {
            val ref = inbox(value.box.sid)._1
            if (blocks.known(value.id)) {
              val returnedBlock:List[Block] = List(blocks.get(value.id))
              send(self,ref,ReturnBlock(returnedBlock,signBox(hash((returnedBlock.map(_.id),0,value.job),serializer),sessionId,keys.sk_sig,keys.pk_sig),value.job))
              if (holderIndex == SharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Returned Block")
              }
            } else {
              val returnedBlock:List[Block] = List()
              send(self,ref,ReturnBlock(returnedBlock,signBox(hash((returnedBlock.map(_.id),0,value.job),serializer),sessionId,keys.sk_sig,keys.pk_sig),value.job))
            }
          } else {println("error: request block invalid box")}
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

      /**block passing, parent ids are requested with increasing depth of chain upto a finite number of attempts*/
    case value:RequestChain => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.box.sid)) {
          val request:Request = (List(value.id),value.depth,value.job)
          if (verifyBox(hash(request,serializer),value.box) && value.depth <= tineMaxDepth) {
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Was Requested Blocks")
            }
            val ref = inbox(value.box.sid)._1
            val startId:BlockId = value.id
            val depth:Int = value.depth
            val job:Int = value.job
            var returnedBlockList:List[Block] = List()
            var id:BlockId = startId
            while (returnedBlockList.length < k_s*depth && blocks.known(id)) {
              returnedBlockList ::= blocks.get(id)
              id = returnedBlockList.head.pid
            }
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Returned Blocks")
            }
            send(self,ref,ReturnBlock(returnedBlockList,signBox(hash((returnedBlockList.map(_.id),0,value.job),serializer),sessionId,keys.sk_sig,keys.pk_sig),value.job))
          } else {println("error:chain request box invalid")}
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

      /**issue a transaction generated by the coordinator and send it to the list of gossipers*/
    case value:IssueTx => {
      if (!actorStalled) {
        value.s match {
          case data:(PublicKeyW,BigInt) => {
            if (holderIndex == SharedData.printingHolder && printFlag) {println(s"Holder $holderIndex Issued Transaction:" + "local state balance:"   +   localState(keys.pkw)._1.toString)}
            wallet.issueTx(data,keys.sk_sig,sig,rng,serializer) match {
              case trans:Transaction => {
                txCounter += 1
                setOfTxs += (trans.sid->trans.nonce)
                memPool += (trans.sid->(trans,0))
                send(self,gossipers, SendTx(trans,signBox(hash(trans,serializer),sessionId,keys.sk_sig,keys.pk_sig)))
              }
              case _ => //{println("Holder "+holderIndex.toString+" tx issue failed")}
            }
          }
          case _ => {println("invalid tx data");SharedData.throwError(holderIndex)}
        }
      } else {
        println("tx issued while stalled");SharedData.throwError(holderIndex)
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

      /**gossip protocol greeting message for populating inbox*/
    case value:Hello => {
      if (!actorStalled) {
        if (gossipers.length < numGossipers + gOff) {
          if (verifyBox(hash(value.id,serializer),value.box)) {
            if (!gossipers.contains(value.id) && inbox.keySet.contains(value.box.sid)) {
              if (holderIndex == SharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Adding Gossiper")
              }
              if (inbox(value.box.sid)._1 == value.id) gossipers = gossipers ++ List(value.id)
              send(self,value.id,Hello(self,signBox(hash(self,serializer), sessionId, keys.sk_sig, keys.pk_sig)))
            }
          }
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }


/************************************************** Diffuse ***********************************************************/

    /**sends holder information for populating inbox*/
    case Diffuse => {
      sendDiffuse(holderId, holders, DiffuseData(self,keys.publicKeys,signBox(hash((self,keys.publicKeys),serializer), sessionId, keys.sk_sig, keys.pk_sig)))
      sender() ! "done"
    }

    /**validates diffused string from other holders and stores in inbox */
    case value:DiffuseData => {
      if (verifyBox(hash((value.ref,value.pks),serializer),value.box) && !inbox.keySet.contains(value.box.sid)) {
        inbox += (value.box.sid->(value.ref,value.pks))
      }
      sender() ! "done"
    }


/************************************************** Coordinator *******************************************************/

      /**allocate arrays and vars of simulation*/
    case value:Initialize => {
      println("Holder "+holderIndex.toString+" starting...")
      tMax = value.tMax
      localChain = Chain((0,genBlockHash))
      chainHistory.update((0,genBlockHash))
      assert(genBlockHash == hash(blocks.get(genBlockHash).prosomoHeader,serializer))
      updateLocalState(localState, Chain(localChain.get(0))) match {
        case value:State => localState = value
        case _ => {
          SharedData.throwError(holderIndex)
          println("error: invalid genesis block")
        }
      }
      println("local state balance:"   +   localState(keys.pkw)._1.toString)
      assert(localState(keys.pkw)._1 > 0)
      eta = eta(localChain, 0, Array())
      history.add(genBlockHash,localState,eta)
      updateWallet
      sender() ! "done"
    }

      /**starts the timer that repeats the update command*/
    case Run => {
      if (!useFencing) timers.startPeriodicTimer(timerKey, Update, updateTime)
    }

      /**sets the initial time*/
    case value:SetClock => {
      t0 = value.t0
      sender() ! "done"
    }

      /**sets the slot from coordinator time*/
    case value:GetTime => if (!actorStalled) {
      globalSlot = ((value.t1 - t0) / slotT).toInt
    }

      /**accepts list of other holders from coordinator */
    case list:List[ActorRef] => {
      holders = list
      if (useGossipProtocol) {
        gossipers = List()
      } else {
        gossipers = gossipSet(holderId,holders)
      }
      var i = 0
      for (holder <- holders) {
        if (self == holder) holderIndex = i
        i += 1
      }
      sender() ! "done"
    }

      /**accepts genesis block from coordinator */
    case gb:GenBlock => {
      genBlockHash = hash(gb.b.prosomoHeader,serializer)
      assert(genBlockHash == gb.b.id)
      assert(verifyBlock(gb.b))
      blocks.add(gb.b)
      sender() ! "done"
    }

      /**when stalled actor will do nothing when messages are received*/
    case StallActor => {
      if (!actorStalled) {actorStalled = true}
      else {actorStalled = false}
      sender() ! "done"
    }

      /**prints inbox */
    case Inbox => {
      var i = 0
      println("Holder "+holderIndex.toString+":"+Base58.encode(sessionId.data))
      for (entry <- inbox) {
        println(i.toString+" "+Base58.encode(entry._1.data))
        i+=1
      }
      println("")
      sender() ! "done"
    }

    case GetBalance => {
      val netAvailable = wallet.getBalance
      val netTotal = wallet.getTotalBalance
      println(s"Holder $holderIndex available balance: $netAvailable , total balance: $netTotal")
    }

      /**prints stats */
    case Verify => {
      val trueChain = verifyChain(localChain, genBlockHash)
      println("Holder "+holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toDoubleString + ", blocks forged = "
        + blocksForged.toString + "\nChain length = " + getActiveSlots(localChain).toString + ", Valid chain = "
        + trueChain.toString)
      var chainBytes:Array[Byte] = Array()
      for (id <- subChain(localChain,0,localSlot-confirmationDepth).ordered) {
        getBlockHeader(id) match {
          case b:BlockHeader => chainBytes ++= FastCryptographicHash(serializer.getBytes(b))
          case _ =>
        }
      }
      println("Public Key: "+Base58.encode(keys.pk_sig++keys.pk_vrf++keys.pk_kes))
      println("Path: "+self.path)
      println("Chain hash: " + Base58.encode(FastCryptographicHash(chainBytes))+"\n")
      if (SharedData.error){
        for (id <- localChain.ordered) {
          if (id._1 > -1) println("H:" + holderIndex.toString + "S:" + id._1.toString + "ID:" + Base58.encode(id._2.data))
        }
        println("e:" + Base58.encode(eta(localChain, currentEpoch)) + "\n")
      }
      sender() ! "done"
    }

      /**prints stats */
    case Status => {
      println("Holder "+holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toDoubleString + ", blocks forged = "
        + blocksForged.toString + "\nChain length = " + getActiveSlots(localChain).toString+", MemPool Size = "+memPool.size+" Num Gossipers = "+gossipers.length.toString)
      var chainBytes:Array[Byte] = Array()
      for (id <- subChain(localChain,0,localSlot-confirmationDepth).ordered) {
        getBlockHeader(id) match {
          case b:BlockHeader => {
            chainBytes ++= FastCryptographicHash(serializer.getBytes(b))
          }
          case _ =>
        }
      }
      SharedData.txCounter += txCounter
      SharedData.setOfTxs ++= setOfTxs
      var txCount = 0
      var allTx:List[Sid] = List()
      var duplicatesFound = false
      var allTxSlots:List[Slot] = List()
      var holderTxOnChain:List[(Sid,Transaction)] = List()
      for (id <- subChain(localChain,1,localSlot).ordered) {
        for (trans<-blocks.getTxs(id)) {
          if (!allTx.contains(trans.sid)) {
            if (trans.sender == keys.pkw) holderTxOnChain ::= (trans.sid,trans)
            allTx ::= trans.sid
            allTxSlots ::= id._1
            txCount+=1
          } else {
            duplicatesFound = true
            val dupIndex = allTx.indexOf(trans.sid)
          }
        }
      }
      val holderTxCount = holderTxOnChain.length
      val holderTxCountTotal = setOfTxs.keySet.size
      val txCountChain = if (holderTxOnChain.isEmpty) {0} else {holderTxOnChain.head._2.nonce}
      val txCountState = math.max(localState(keys.pkw)._3-1,0)
      println(s"Tx Counts in state and chain: $txCountState, $txCountChain")
      println(s"Transactions on chain: $holderTxCount / $holderTxCountTotal Total: $txCount Duplicates: $duplicatesFound")
      println("Chain hash: " + Base58.encode(FastCryptographicHash(chainBytes))+"\n")
      sender() ! "done"
    }

      /**writes data point to file*/
    case value:WriteFile => if (!actorStalled) {
      value.fw match {
        case fileWriter: BufferedWriter => {
          val fileString = (
            holderIndex.toString + " "
              + globalSlot.toString + " "
              + keys.alpha.toDoubleString + " "
              + blocksForged.toString + " "
              + getActiveSlots(localChain).toString + " "
              + "\n"
            )
          fileWriter.write(fileString)
        }
        case _ => println("error: data file writer not initialized")
      }
    }

      /**accepts coordinator ref*/
    case value:CoordRef => {
      coordinatorRef = value.ref
      sender() ! "done"
    }

      /**accepts router ref*/
    case value:RouterRef => {
      routerRef = value.ref
      sender() ! "done"
    }

      /**sets new list of holders resets gossipers*/
    case value:Party => {
      value.list match {
        case list: List[ActorRef] => {
          holders = list
          if (useGossipProtocol) {
            gossipers = List()
            numHello = 0
          } else {
            gossipers = gossipSet(holderId,holders)
          }
          if (value.clear) inbox = Map()
          diffuseSent = false
        }
        case _ =>
      }
      sender() ! "done"
    }

    case RequestGossipers => {
      sender() ! GetGossipers(gossipers)
    }

    case RequestState => {
      sender() ! GetState(stakingState)
    }

    case RequestBlockTree => {
      sender() ! GetBlockTree(blocks,chainHistory)
    }

    case RequestKeys => {
      sender() ! diffuse(bytes2hex(keys.pk_sig)+";"+bytes2hex(keys.pk_vrf)+";"+bytes2hex(keys.pk_kes), s"{$holderId}", keys.sk_sig)
    }

    case unknown:Any => if (!actorStalled) {
      print("received unknown message ")
      if (sender() == coordinatorRef) {
        print("from coordinator")
      }
      if (sender() == routerRef) {
        print("from router")
      }
      if (holders.contains(sender())) {
        print("from holder "+holders.indexOf(sender()).toString)
      }
      println(": "+unknown.getClass.toString)
    }
  }
}

object Stakeholder {
  def props(seed:Array[Byte]): Props = Props(new Stakeholder(seed))
}

