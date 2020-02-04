package prosomo

import akka.actor.{Actor, ActorPath, ActorRef, Props, Timers}
import bifrost.crypto.hash.FastCryptographicHash
import util.control.Breaks._
import java.io.BufferedWriter
import io.iohk.iodb.ByteArrayWrapper
import scala.math.BigInt
import scala.util.Random
import scorex.crypto.encode.Base58
import scala.util.{Try, Success, Failure}

/**
  * Stakeholder actor that executes the staking protocol and communicates with other stakeholders,
  * sends the coordinator the public key upon instantiation and gets the genesis block from coordinator
  */

class Stakeholder(seed:Array[Byte]) extends Actor
  with Timers
  with Methods
  with StakeholderVariables {
  val holderId:ActorPath = self.path
  val sessionId:Sid = ByteArrayWrapper(FastCryptographicHash(holderId.toString))
  rng = new Random(BigInt(seed).toLong)
  val phase:Double = rng.nextDouble
  var chainUpdateLock = false
  val keys:Keys = Keys(seed,sig,vrf,kes,0)
  val wallet:Wallet = new Wallet(keys.pkw)

  private case object timerKey


  /*************************Honest************************************/

  def blockInfo:String = {
    "forger_index:"+holderIndex.toString+",adversarial:"+adversary.toString+",eta:"+Base58.encode(eta)+",epoch:"+currentEpoch.toString
  }

  /**determines eligibility for a stakeholder to be a slot leader then calculates a block with epoch variables */
  def forgeBlock(forgerKeys:Keys) = {
    val slot = localSlot
    val pi_y: Pi = vrf.vrfProof(forgerKeys.sk_vrf, eta ++ serialize(slot) ++ serialize("TEST"))
    val y: Rho = vrf.vrfProofToHash(pi_y)
    if (compare(y, forgerKeys.threshold)) {
      roundBlock = {
        val pb:Block = getBlock(localChain(lastActiveSlot(localChain,localSlot-1))) match {case b:Block => b}
        val bn:Int = pb._9 + 1
        val ps:Slot = pb._3
        val blockBox: Box = signBox((forgeBytes,BigDecimal(forgerReward).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt), sessionId, forgerKeys.sk_sig, forgerKeys.pk_sig)
        val pi: Pi = vrf.vrfProof(forgerKeys.sk_vrf, eta ++ serialize(slot) ++ serialize("NONCE"))
        val rho: Rho = vrf.vrfProofToHash(pi)
        val h: Hash = hash(pb)
        val ledger = blockBox::chooseLedger(forgerKeys.pkw,memPool,localState)
        val cert: Cert = (forgerKeys.pk_vrf, y, pi_y, forgerKeys.pk_sig, forgerKeys.threshold,blockInfo)
        val kes_sig: KesSignature = forgerKeys.sk_kes.sign(kes,h.data++serialize(ledger)++serialize(slot)++serialize(cert)++rho++pi++serialize(bn)++serialize(ps))
        (h, ledger, slot, cert, rho, pi, kes_sig, forgerKeys.pk_kes,bn,ps)
      }
    } else {
      roundBlock = -1
    }
    roundBlock match {
      case b: Block => {
        val hb = hash(b)
        val bn = b._9
        if (printFlag) {
          println("Holder " + holderIndex.toString + s" forged block $bn with id:"+Base58.encode(hb.data))
        }
        blocks.update(localSlot, blocks(localSlot) + (hb -> b))
        localChain.update(localSlot, (localSlot, hb))
        chainHistory.update(localSlot,(localSlot, hb)::chainHistory(localSlot))
        send(self,gossipers, SendBlock(signBox((b,(localSlot, hb)), sessionId, keys.sk_sig, keys.pk_sig)))
        blocksForged += 1
        updateLocalState(localState, Array(localChain(localSlot))) match {
          case value:State => localState = value
          case _ => {
            sharedData.throwError
            println("error: invalid ledger in forged block")
          }
        }
        history.add(hb,localState,eta)
        updateWallet
        trimMemPool
      }
      case _ =>
    }
  }

  def updateTine(inputTine:Chain): (Chain,Slot) = {
    var foundAncestor = true
    var tine:Chain = inputTine
    var prefix:Slot = 0
    if (localChain(tine.head._1) == tine.head) {
      (tine,-1)
    } else {
      breakable{
        while(foundAncestor) {
          getParentId(tine.head) match {
            case pb:BlockId => {
              tine = Array(pb) ++ tine
              if (tine.head == localChain(tine.head._1)) {
                prefix = tine.head._1
                tine = tine.tail
                break
              }
              if (tine.head._1 == 0) {
                prefix = 0
                tine = tine.tail
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
    var id = localChain(lastActiveSlot(localChain,localSlot))
    val bn = {getBlock(id) match {case b:Block => b._9}}
    if (bn == 0) {
      getBlock(id) match {
        case b:Block => {
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
          id = getParentId(id) match {case value:BlockId => value}
          getBlock(id) match {
            case b:Block => {
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
    for (trans <- wallet.getPending(localState)) {
      if (!memPool.keySet.contains(trans._4)) memPool += (trans._4->(trans,0))
      send(self,gossipers, SendTx(trans))
    }
  }

  def buildTine(job:(Int,(Chain,Int,Int,Int,ActorRef))): Unit = {
    val entry = job._2
    var foundAncestor = true
    var tine:Chain = entry._1
    var counter:Int = entry._2
    val previousLen:Int = entry._3
    val totalTries:Int = entry._4
    val ref:ActorRef = entry._5
    var prefix:Slot = 0
    breakable{
      while(foundAncestor) {
        getParentId(tine.head) match {
          case pb:BlockId => {
            tine = Array(pb) ++ tine
            if (tine.head == localChain(tine.head._1)) {
              prefix = tine.head._1
              tine = tine.tail
              break
            }
            if (tine.head._1 == 0) {
              prefix = 0
              tine = tine.tail
              break
            }
          }
          case _ => {
            if (tine.length == previousLen) {counter+=1} else {counter=0}
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
        if (holderIndex == sharedData.printingHolder && printFlag) println("Holder " + holderIndex.toString + " Dropping Old Tine")
        tines -= job._1
      } else {
        tines -= job._1
        tines += (job._1 -> (tine,counter,tine.length,totalTries+1,ref))
        if (totalTries > tineMaxTries) {
          if (holderIndex == sharedData.printingHolder && printFlag) println(
            "Holder " + holderIndex.toString + " Looking for Parent Blocks C:"+counter.toString+"L:"+tine.length
          )
          val depth:Int = if (totalTries - tineMaxTries < tineMaxDepth) {
            totalTries - tineMaxTries
          } else {
            tineMaxDepth
          }
          val request:ChainRequest = (tine.head,depth,job._1)
          send(self,ref, RequestChain(signBox(request,sessionId,keys.sk_sig,keys.pk_sig)))
        } else {
          if (holderIndex == sharedData.printingHolder && printFlag) println("Holder " + holderIndex.toString + " Looking for Parent Block C:"+counter.toString+"L:"+tine.length)
          val request:BlockRequest = (tine.head,job._1)
          send(self,ref, RequestBlock(signBox(request,sessionId,keys.sk_sig,keys.pk_sig)))
        }
      }
    }
  }

  /**main chain selection routine, maxvalid-bg*/
  def maxValidBG = {
    val prefix:Slot = candidateTines.last._2
    val tine:Chain = expand(candidateTines.last._1,prefix,localSlot)
    val job:Int = candidateTines.last._3
    var trueChain = false
    val s1 = tine.last._1
    val bnt = {getBlock(tine(lastActiveSlot(tine,localSlot-prefix-1))) match {case b:Block => b._9}}
    val bnl = {getBlock(localChain(lastActiveSlot(localChain,localSlot))) match {case b:Block => b._9}}
    if(s1 - prefix < k_s && bnl < bnt) {
      trueChain = true
    } else {
      val slotsTine = getActiveSlots(subChain(tine,0,slotWindow))
      val slotsLocal = getActiveSlots(subChain(localChain,prefix+1,prefix+1+slotWindow))
      if (slotsLocal < slotsTine) {
        trueChain = true
      }
    }
    if (trueChain) {
      trueChain &&= verifySubChain(tine,prefix)
    }
    if (trueChain) {
      if (holderIndex == sharedData.printingHolder && printFlag) println("Holder " + holderIndex.toString + " Adopting Chain")
      collectLedger(subChain(localChain,prefix+1,localSlot))
      collectLedger(tine)
      for (id <- subChain(localChain,prefix+1,localSlot)) {
        getBlock(id) match {
          case b:Block => {
            val ledger:Ledger = b._2
            wallet.add(ledger)
          }
          case _ =>
        }
      }
      for (i <- prefix+1 to localSlot) {
        val id = tine(i-prefix-1)
        if (id._1 > -1) {
          assert(id._1 == i)
          chainHistory.update(id._1,id::{chainHistory(id._1).tail})
          localChain.update(id._1,id)
          getBlock(id) match {
            case b:Block => {
              val blockLedger = b._2
              for (entry<-blockLedger.tail) {
                entry match {
                  case trans:Transaction => {
                    if (memPool.keySet.contains(trans._4)) {
                      memPool -= trans._4
                    }
                  }
                  case _ =>
                }
              }
            }
            case _ =>
          }
        } else {
          chainHistory.update(i,(-1,ByteArrayWrapper(Array()))::chainHistory(i))
          localChain.update(i,(-1,ByteArrayWrapper(Array())))
        }
      }
      candidateTines = candidateTines.dropRight(1)
      var newCandidateTines:Array[(Chain,Slot,Int)] = Array()
      for (entry <- candidateTines) {
        val newTine = updateTine(Array(entry._1.last))
        if (newTine._2 > 0) {
          newCandidateTines = newCandidateTines ++ Array((newTine._1,newTine._2,entry._3))
        }
      }
      candidateTines = newCandidateTines
      updateWallet
      trimMemPool
    } else {
      collectLedger(tine)
      for (id <- subChain(localChain,prefix+1,localSlot)) {
        if (id._1 > -1) {
          getBlock(id) match {
            case b: Block => {
              val blockState = b._2
              for (entry <- blockState.tail) {
                entry match {
                  case trans: Transaction =>  {
                    if (memPool.keySet.contains(trans._4)){
                      memPool -= trans._4
                    }
                  }
                  case _ =>
                }
              }
            }
            case _ =>
          }
        }
      }
      candidateTines = candidateTines.dropRight(1)
    }
  }

  def validateChainIds = {
    var pid = localChain.head
    for (id <- localChain.tail) {
      getParentId(id) match {
        case bid:BlockId => {
          if (bid == pid) {
            pid = id
          } else {
            println(s"Holder $holderIndex error: pid mismatch in local chain")
            sharedData.throwError
          }
        }
        case _ =>
      }
    }
  }

  /**slot routine, called every time currentSlot increments*/
  def updateSlot = {
    time(
      updateEpoch
    )
    if (localSlot == globalSlot) {
      time(
        if (keys.sk_kes.time(kes) < localSlot) {
          if (holderIndex == sharedData.printingHolder && printFlag && localSlot%epochLength == 0) {
            println("Current Epoch = " + currentEpoch.toString)
            println("Holder " + holderIndex.toString + " alpha = " + keys.alpha.toString+"\nEta:"+Base58.encode(eta))
          }
          roundBlock = 0
          if (holderIndex == sharedData.printingHolder) println(Console.CYAN + "Slot = " + localSlot.toString + " on block " + Base58.encode(localChain(lastActiveSlot(localChain,globalSlot))._2.data) + Console.WHITE)
          keys.sk_kes.update(kes, localSlot)
          if (useGossipProtocol) {
            val newOff = (numGossipers*math.sin(2.0*math.Pi*(globalSlot.toDouble/100.0+phase))/2.0).toInt
            if (newOff != gOff) {
              if (gOff < newOff) numHello = 0
              gOff = newOff
            }
            if (gossipers.length < numGossipers + gOff && numHello < 1) {
              send(self,rng.shuffle(holders.filter(_!=self)),Hello(signBox(self, sessionId, keys.sk_sig, keys.pk_sig)))
              numHello += 1
            } else if (gossipers.length > numGossipers + gOff) {
              gossipers = rng.shuffle(gossipers).take(numGossipers + gOff)
            }
          }
        }
      )
    }
    updateLocalState(localState, Array(localChain(localSlot))) match {
      case value:State => localState = value
      case _ => {
        sharedData.throwError
        println("error: invalid block ledger on chain")
      }
    }
    if (dataOutFlag && globalSlot % dataOutInterval == 0) {
      coordinatorRef ! WriteFile
    }
  }

  /**epoch routine, called every time currentEpoch increments*/
  def updateEpoch = {
    if (localSlot / epochLength > currentEpoch) {
      currentEpoch = localSlot / epochLength
      stakingState = {
        if (currentEpoch > 1) {
          history.get(localChain((currentEpoch-1)*epochLength)._2) match {
            case value:(State,Eta) => {
              value._1
            }
            case _ => {
              Map()
            }
          }
        } else {
          history.get(localChain(0)._2) match {
            case value:(State,Eta) => {
              value._1
            }
            case _ => {
              Map()
            }
          }
        }
      }
      keys.alpha = relativeStake(keys.pkw, stakingState)
      keys.threshold = phi(keys.alpha, f_s)
      if (currentEpoch > 0) {
        eta = eta(localChain, currentEpoch, eta)
      }
    }
  }

  def update = { if (sharedData.error) {actorStalled = true}
    if (!actorStalled) {
      if (!updating) {
        updating = true
        if (globalSlot > tMax || sharedData.killFlag) {
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
            if (holderIndex == sharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Checking Tine")
            }
            time(maxValidBG)
            while (globalSlot > localSlot) {
              localSlot += 1
              updateSlot
            }
          } else if (useFencing && chainUpdateLock) {
            if (candidateTines.isEmpty) {
              chainUpdateLock = false
            } else {
              if (holderIndex == sharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Checking Tine")
              }
              time(maxValidBG)
            }
          }
        }
        updating = false
      }
    }
  }

  /************************Adversarial********************************/

  var honestPrefix:BlockId = (-1,ByteArrayWrapper(Array()))
  var covertHead:BlockId = (-1,ByteArrayWrapper(Array()))
  var covertTine:Chain = Array()
  var leaderPredict:Map[Slot,Boolean] = Map()
  var allTines:Set[BlockId] = Set()
  val numFutureSlots = 100
  val probability_threshold = 0.5
  val maxCovertLength = 10

  def leaderTest(forgerKeys:Keys,slot:Int,etaIn:Eta):Boolean = {
    val pi_y: Pi = vrf.vrfProof(forgerKeys.sk_vrf, etaIn ++ serialize(slot) ++ serialize("TEST"))
    val y: Rho = vrf.vrfProofToHash(pi_y)
    compare(y, forgerKeys.threshold)
  }

  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n-1)
  }

  // probability of no blocks forged by honest parties
  def theta = {
    math.pow(1.0-f_s,1.0-keys.alpha)
  }

  def bernoulli_probability(p:Double,n:Int,m:Int):Double = {
    (factorial(n).toDouble/(factorial(m)*factorial(n-m)).toDouble)*(math.pow(p,m)*math.pow(1-p,n-m))
  }

  def predictive_selfish_mining_logic:Boolean = {
    var out = false
    predictLeaderSlots(localSlot)
    if (covertTine.isEmpty) {
      honestPrefix = localChain(lastActiveSlot(localChain,localSlot))
      covertHead = honestPrefix
    }
    val honestTineLength:Int = if (honestPrefix._1 < localSlot) {
      getActiveSlots(subChain(localChain,honestPrefix._1+1,localSlot))
    } else {
      0
    }
    val covertTineLength:Int = covertTine.length
    var intervals:Array[Int] = Array()
    var i = 0
    for (j <- localSlot+1 to localSlot + numFutureSlots) {
      i = i + 1
      if (leaderPredict(j)) {
        intervals = Array(i) ++ intervals
        i = 0
      }
    }
    intervals.length match {
      case 0 => sendCovertTine
      case _ => {
        val expectedHonest:Double = (1-theta)*intervals.sum
        val expectedCovert:Double = intervals.length
        if (expectedCovert+covertTineLength > expectedHonest+honestTineLength && covertTineLength < maxCovertLength) {
          if (covertTine.isEmpty && leaderPredict(localSlot)) {
            println(Console.RED + s"Holder $holderIndex starting covert Tine" + Console.WHITE)
            out = true
          } else if (!covertTine.isEmpty) {
            println(Console.RED + s"Covert L:$covertTineLength Honest L:$honestTineLength" + Console.WHITE)
            out = true
          }
        } else {
          sendCovertTine
        }
      }
    }
    out
  }

  def sendCovertTine = {
    if (!covertTine.isEmpty) {
      val b = getBlock(covertHead) match {
        case value:Block => value
      }
      val jobNumber = tineCounter
      tines += (jobNumber -> (Array(covertHead),0,0,0,self))
      buildTine((jobNumber,tines(jobNumber)))
      tineCounter += 1
      send(self,gossipers, SendBlock(signBox((b,covertHead), sessionId, keys.sk_sig, keys.pk_sig)))
      println(Console.RED + s"Holder $holderIndex released covert Tine" + Console.WHITE)
      covertTine = Array()
    }
  }

  def forgeBlock(forgerKeys:Keys,pb:Block,slot:Int,stateIn:State,etaIn:Eta):Block = {
    val pi_y: Pi = vrf.vrfProof(forgerKeys.sk_vrf, etaIn ++ serialize(slot) ++ serialize("TEST"))
    val y: Rho = vrf.vrfProofToHash(pi_y)
    val bn:Int = pb._9 + 1
    val ps:Slot = pb._3
    val blockBox: Box = signBox((forgeBytes,BigDecimal(forgerReward).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt), sessionId, forgerKeys.sk_sig, forgerKeys.pk_sig)
    val pi: Pi = vrf.vrfProof(forgerKeys.sk_vrf, etaIn ++ serialize(slot) ++ serialize("NONCE"))
    val rho: Rho = vrf.vrfProofToHash(pi)
    val h: Hash = hash(pb)
    val ledger = blockBox::chooseLedger(forgerKeys.pkw,memPool,stateIn)
    val cert: Cert = (forgerKeys.pk_vrf, y, pi_y, forgerKeys.pk_sig, forgerKeys.threshold,blockInfo+",tag:covert")
    val sig: KesSignature = forgerKeys.sk_kes.sign(kes,h.data++serialize(ledger)++serialize(slot)++serialize(cert)++rho++pi++serialize(bn)++serialize(ps))
    (h, ledger, slot, cert, rho, pi, sig, forgerKeys.pk_kes,bn,ps)
  }

  def covertlyForge(keys:Keys) = {

    if (predictive_selfish_mining_logic) {
      val data:(State,Eta) = history.get(covertHead._2) match {case value:(State,Eta) => value}
      if (leaderTest(keys,localSlot,eta)) {
        val parentBlock:Block = getBlock(covertHead) match {case value:Block => value}
        //(forgerKeys:Keys,pb:Block,slot:Int,stateIn:State,etaIn:Eta)
        val covertBlock = forgeBlock(keys,parentBlock,localSlot,data._1,eta)
        roundBlock = covertBlock
      } else {
        roundBlock = -1
      }
      roundBlock match {
        case b: Block => {
          val hb = hash(b)
          val bn = b._9
          if (printFlag) {
            println("Holder " + holderIndex.toString + s" forged block $bn with id:"+Base58.encode(hb.data))
          }
          blocks.update(localSlot, blocks(localSlot) + (hb -> b))
          blocksForged += 1
          updateLocalState(data._1, Array((localSlot,hb))) match {
            case value:State => {
              covertHead = (localSlot,hb)
              covertTine = covertTine ++ Array(covertHead)
              history.add(hb,value,eta)
            }
            case _ => {
              sharedData.throwError
              println("error: invalid ledger in forged block")
            }
          }
        }
        case _ =>
      }
    } else {
      forgeBlock(keys)
    }
  }

  def forgeEveryTine(keys:Keys) = {
    if (leaderTest(keys,localSlot,eta)) {
      var newTines:Set[BlockId] = Set()
      for (entry <- allTines) {
        val data: (State, Eta) = history.get(entry._2) match {
          case value: (State, Eta) => value
        }
        val parentBlock: Block = getBlock(entry) match {
          case value: Block => value
        }
        forgeBlock(keys, parentBlock, localSlot, data._1, eta) match {
          case b: Block => {
            val hb = hash(b)
            val bn = b._9
            if (printFlag) {
              println(Console.RED + "Holder " + holderIndex.toString + s" forged block $bn with id:" + Base58.encode(hb.data) + Console.WHITE)
            }
            blocks.update(localSlot, blocks(localSlot) + (hb -> b))
            blocksForged += 1
            updateLocalState(data._1, Array((localSlot, hb))) match {
              case value: State => {
                history.add(hb, value, eta)
                newTines ++= Set((localSlot,hb))
                send(self,holders, SendBlock(signBox((b,(localSlot, hb)), sessionId, keys.sk_sig, keys.pk_sig)))
              }
              case _ => {
                sharedData.throwError
                println(Console.RED + "error: invalid ledger in forged block" + Console.WHITE)
              }
            }
          }
          case _ =>
        }
      }
      allTines = newTines
    }
    roundBlock = -1
  }

  def predictLeaderSlots(currentSlot:Slot) = {
    if (leaderPredict.isEmpty) {
      for (i <- currentSlot to currentSlot + numFutureSlots) {
        leaderPredict += (i -> leaderTest(keys,i,eta))
      }
    } else {
      for (entry <- leaderPredict) {
        if (entry._1 < currentSlot || entry._1 >= currentSlot + numFutureSlots) leaderPredict -= entry._1
      }
      leaderPredict += (currentSlot + numFutureSlots -> leaderTest(keys,currentSlot + numFutureSlots,eta))
    }
  }

  def updateAdversary = {
    if (sharedData.error) {
      actorStalled = true
    }
    if (!actorStalled) {
      if (!updating) {
        updating = true
        if (globalSlot > tMax || sharedData.killFlag) {
          timers.cancelAll
        } else if (diffuseSent) {
          if (!useFencing) {
            coordinatorRef ! GetTime
          }
          if (globalSlot > localSlot) {
            while (globalSlot > localSlot) {
              localSlot += 1
              updateSlot
            }
          } else if (roundBlock == 0 && candidateTines.isEmpty) {
            if (forgeAll) {
              forgeEveryTine(keys)
            } else if (covert) {
              covertlyForge(keys)
            } else {
              forgeBlock(keys)
            }
            if (useFencing) {
              routerRef ! (self, "updateSlot")
            }
          } else if (!useFencing && candidateTines.nonEmpty) {
            if (holderIndex == sharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Checking Tine")
            }
            assert(history.get(candidateTines.last._1.last._2) match {
              case value:(State, Eta) => true
              case _ => {
                sharedData.throwError
                false
              }
            })
            if (forgeAll) allTines ++= Set(candidateTines.last._1.last)
            time(maxValidBG)
            while (globalSlot > localSlot) {
              localSlot += 1
              updateSlot
            }
          } else if (useFencing && chainUpdateLock) {
            if (candidateTines.isEmpty) {
              chainUpdateLock = false
            } else {
              if (holderIndex == sharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Checking Tine")
              }
              time(maxValidBG)
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
        updateAdversary
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
        value.s match {
          case trans:Transaction => {
            if (!memPool.keySet.contains(trans._4) && localState.keySet.contains(trans._1)) {
              if (localState(trans._1)._3 <= trans._5) {
                if (verifyTransaction(trans)) {
                  memPool += (trans._4->(trans,0))
                  send(self,gossipers, SendTx(value.s))
                }
              }
            }
          }
          case _ =>
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

      /**block passing, new blocks delivered are added to list of tines and then sent to gossipers*/
    case value:SendBlock => {
      if (!actorStalled) {
        value.s match {
          case s:Box => if (inbox.keySet.contains(s._2)) {
            s._1 match {
              case bInfo: (Block,BlockId) => {
                val bid:BlockId = bInfo._2
                val foundBlock = blocks(bid._1).contains(bid._2)
                if (!foundBlock) {
                  val b:Block = bInfo._1
                  val bHash = hash(b)
                  val bSlot = b._3
                  if (verifyBox(s) && verifyBlock(b) && bHash == bid._2 && bSlot == bid._1) {
                    if (!foundBlock) blocks.update(bSlot, blocks(bSlot) + (bHash -> b))
                    if (!foundBlock && bSlot <= globalSlot) {
                      if (holderIndex == sharedData.printingHolder && printFlag) {
                        println("Holder " + holderIndex.toString + " Got New Tine")
                      }
                      val newId = (bSlot, bHash)
                      send(self,gossipers, SendBlock(signBox((b,newId), sessionId, keys.sk_sig, keys.pk_sig)))
                      val jobNumber = tineCounter
                      tines += (jobNumber -> (Array(newId),0,0,0,inbox(s._2)._1))
                      buildTine((jobNumber,tines(jobNumber)))
                      tineCounter += 1
                    }
                  }
                }
              }
              case _ =>
            }
          }
          case _ =>
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
        value.s match {
          case s:Box => if (inbox.keySet.contains(s._2)) {
            s._1 match {
              case returnedBlocks: (Int,List[(Block,BlockId)]) => {
                if (holderIndex == sharedData.printingHolder && printFlag) {
                  println("Holder " + holderIndex.toString + " Got Blocks")
                }
                val jobNumber:Int = returnedBlocks._1
                val bList = returnedBlocks._2
                for (bInfo <- bList) {
                  val bid:BlockId = bInfo._2
                  val foundBlock = blocks(bid._1).contains(bid._2)
                  if (!foundBlock) {
                    val b:Block = bInfo._1
                    val bHash = hash(b)
                    val bSlot = b._3
                    if (verifyBox(s) && verifyBlock(b) && bHash == bid._2 && bSlot == bid._1) {
                      blocks.update(bSlot, blocks(bSlot) + (bHash -> b))
                    }
                  }
                }
                if (tines.keySet.contains(jobNumber)) buildTine((jobNumber,tines(jobNumber)))
              }
              case nullBlock:NullBlock => {
                val jobNumber = nullBlock.job
                if (tines.keySet.contains(jobNumber)) buildTine((jobNumber,tines(jobNumber)))
              }
              case _ =>
            }
          }
          case _ =>
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
        value.s match {
          case s:Box => {
            if (inbox.keySet.contains(s._2)) {
              if (holderIndex == sharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Was Requested Block")
              }
              val ref = inbox(s._2)._1
              s._1 match {
                case request:BlockRequest => {
                  val id:BlockId = request._1
                  val job:Int = request._2
                  if (blocks(id._1).contains(id._2)) {
                    if (verifyBox(s)) {
                      val returnedBlock = blocks(id._1)(id._2)
                      send(self,ref,ReturnBlock(signBox((job,List((returnedBlock,id))),sessionId,keys.sk_sig,keys.pk_sig)))
                      if (holderIndex == sharedData.printingHolder && printFlag) {
                        println("Holder " + holderIndex.toString + " Returned Block")
                      }
                    }
                  } else {
                    send(self,ref,ReturnBlock(signBox(NullBlock(job),sessionId,keys.sk_sig,keys.pk_sig)))
                  }
                }
                case _ =>
              }
            }
          }
          case _ =>
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

      /**block passing, parent ids are requested with increasing depth of chain upto a finite number of attempts*/
    case value:RequestChain => {
      if (!actorStalled) {
        value.s match {
          case s:Box => {
            if (inbox.keySet.contains(s._2)) {
              if (holderIndex == sharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Was Requested Blocks")
              }
              val ref = inbox(s._2)._1
              s._1 match {
                case request:ChainRequest => {
                  val startId:BlockId = request._1
                  val depth:Int = request._2
                  val job:Int = request._3
                  var parentFound = blocks(startId._1).contains(startId._2)
                  var returnedBlockList:List[(Block,BlockId)] = List()
                  if (depth <= tineMaxDepth && parentFound) {
                    if (verifyBox(s)) {
                      var id = startId
                      while (parentFound && returnedBlockList.length < k_s*depth) {
                        parentFound = getBlock(id) match {
                          case b:Block => {
                            returnedBlockList ::= (b,id)
                            id = getParentId(b)
                            true
                          }
                          case _ => false
                        }
                      }
                      if (holderIndex == sharedData.printingHolder && printFlag) {
                        println("Holder " + holderIndex.toString + " Returned Blocks")
                      }
                    }
                  }
                  if (returnedBlockList.nonEmpty) {
                    send(self,ref,ReturnBlock(signBox((job,returnedBlockList),sessionId,keys.sk_sig,keys.pk_sig)))
                  } else {
                    send(self,ref,ReturnBlock(signBox(NullBlock(job),sessionId,keys.sk_sig,keys.pk_sig)))
                  }
                }
                case _ =>
              }
            }
          }
          case _ =>
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
            if (holderIndex == sharedData.printingHolder && printFlag) {println(s"Holder $holderIndex Issued Transaction")}
            wallet.issueTx(data,keys.sk_sig,sig,rng) match {
              case trans:Transaction => {
                txCounter += 1
                setOfTxs += (trans._4->trans._5)
                memPool += (trans._4->(trans,0))
                send(self,gossipers, SendTx(trans))
              }
              case _ => //{println("Holder "+holderIndex.toString+" tx issue failed")}
            }
          }
          case _ => {println("invalid tx data");sharedData.throwError}
        }
      } else {
        println("tx issued while stalled");sharedData.throwError
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

      /**gossip protocol greeting message for populating inbox*/
    case value:Hello => {
      if (!actorStalled) {
        if (gossipers.length < numGossipers + gOff) {
          value.id match {
            case id:Box => {
              id._1 match {
                case ref:ActorRef => {
                  if (verifyBox(id)) {
                    if (!gossipers.contains(ref) && inbox.keySet.contains(id._2)) {
                      if (holderIndex == sharedData.printingHolder && printFlag) {
                        println("Holder " + holderIndex.toString + " Adding Gossiper")
                      }
                      if (inbox(id._2)._1 == ref) gossipers = gossipers ++ List(ref)
                      send(self,ref,Hello(signBox(self, sessionId, keys.sk_sig, keys.pk_sig)))
                    }
                  }
                }
                case _ =>
              }

            }
            case _ =>
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
      sendDiffuse(holderId, holders, signBox((self,keys.publicKeys), sessionId, keys.sk_sig, keys.pk_sig))
      sender() ! "done"
    }

    /**validates diffused string from other holders and stores in inbox */
    case value:Box => {
      if (verifyBox(value) && !inbox.keySet.contains(value._2)) {
        val sid = value._2
        value._1 match {
          case d:(ActorRef,PublicKeys) => inbox += (sid->d)
          case _ =>
        }
      }
      sender() ! "done"
    }


/************************************************** Coordinator *******************************************************/

      /**allocate arrays and vars of simulation*/
    case value:Initialize => {
      println("Holder "+holderIndex.toString+" starting...")
      tMax = value.tMax
      blocks = blocks++Array.fill(tMax){Map[ByteArrayWrapper,Block]()}
      localChain = Array((0,genBlockHash))++Array.fill(tMax){(-1,ByteArrayWrapper(Array()))}
      chainHistory = Array(List((0,genBlockHash)))++Array.fill(tMax){List((-1,ByteArrayWrapper(Array())))}
      assert(genBlockHash == hash(blocks(0)(genBlockHash)))
      updateLocalState(localState, Array(localChain(0))) match {
        case value:State => localState = value
        case _ => {
          sharedData.throwError
          println("error: invalid genesis block")
        }
      }
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
      genBlock = gb.b
      genBlock match {
        case b: Block => {
          genBlockHash = hash(b)
          blocks = Array(Map(genBlockHash->b))
        }
        case _ => println("error")
      }
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
      println("Holder "+holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toString + ", blocks forged = "
        + blocksForged.toString + "\nChain length = " + getActiveSlots(localChain).toString + ", Valid chain = "
        + trueChain.toString)
      var chainBytes:Array[Byte] = Array()
      for (id <- subChain(localChain,0,localSlot-confirmationDepth)) {
        getBlock(id) match {
          case b:Block => chainBytes ++= FastCryptographicHash(serialize(b))
          case _ =>
        }
      }
      println("Public Key: "+Base58.encode(keys.pk_sig++keys.pk_vrf++keys.pk_kes))
      println("Path: "+self.path)
      println("Chain hash: " + Base58.encode(FastCryptographicHash(chainBytes))+"\n")
      if (sharedData.error){
        for (id <- localChain) {
          if (id._1 > -1) println("H:" + holderIndex.toString + "S:" + id._1.toString + "ID:" + Base58.encode(id._2.data))
        }
        println("e:" + Base58.encode(eta(localChain, currentEpoch)) + "\n")
      }
      sender() ! "done"
    }

      /**prints stats */
    case Status => {
      println("Holder "+holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toString + ", blocks forged = "
        + blocksForged.toString + "\nChain length = " + getActiveSlots(localChain).toString+", MemPool Size = "+memPool.size+" Num Gossipers = "+gossipers.length.toString)
      var chainBytes:Array[Byte] = Array()
      for (id <- subChain(localChain,0,localSlot-confirmationDepth)) {
        getBlock(id) match {
          case b:Block => {
            chainBytes ++= FastCryptographicHash(serialize(b))
          }
          case _ =>
        }
      }
      sharedData.txCounter += txCounter
      sharedData.setOfTxs ++= setOfTxs
      var txCount = 0
      var allTx:List[Sid] = List()
      var duplicatesFound = false
      var allTxSlots:List[Slot] = List()
      var holderTxOnChain:List[(Sid,Transaction)] = List()
      for (id <- subChain(localChain,0,localSlot)) {
        getBlock(id) match {
          case b:Block => {
            val state = b._2
            for (entry<-state) {
              entry match {
                case trans:Transaction => {
                  if (!allTx.contains(trans._4)) {
                    if (trans._1 == keys.pkw) holderTxOnChain ::= (trans._4,trans)
                    allTx ::= trans._4
                    allTxSlots ::= b._3
                    txCount+=1
                  } else {
                    duplicatesFound = true
                    val dupIndex = allTx.indexOf(trans._4)
                  }
                }
                case _ =>
              }
            }
          }
          case _ =>
        }
      }
      val holderTxCount = holderTxOnChain.length
      val holderTxCountTotal = setOfTxs.keySet.size
      val txCountChain = if (holderTxOnChain.isEmpty) {0} else {holderTxOnChain.head._2._5}
      val txCountState = math.max(localState(keys.pkw)._3-1,0)
      println(s"Tx Counts in state and chain: $txCountState, $txCountChain")
      println(s"Transactions on chain: $holderTxCount / $holderTxCountTotal Total: $txCount Duplicates: $duplicatesFound")
      println("Chain hash: " + Base58.encode(FastCryptographicHash(chainBytes))+"\n")
      if (false){
        for (id <- localChain) {
          if (id._1 > -1) {
            println("S:" + id._1.toString)
            getBlock(id) match {
              case b:Block => {
                for (entry<-b._2) {
                  entry match {
                    case trans:Transaction => println(Base58.encode(trans._4.data)+":"+trans._3.toString)
                    case _ =>
                  }
                }
              }
              case _ => println("error")
            }
          }
        }
      }
      sender() ! "done"
    }

      /**writes data point to file*/
    case value:WriteFile => if (!actorStalled) {
      value.fw match {
        case fileWriter: BufferedWriter => {
          val fileString = (
            holderIndex.toString + " "
              + globalSlot.toString + " "
              + keys.alpha.toString + " "
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
      value.ref match {
        case r: ActorRef => coordinatorRef = r
        case _ =>
      }
      sender() ! "done"
    }

      /**accepts router ref*/
    case value:RouterRef => {
      value.ref match {
        case r: ActorRef => routerRef = r
        case _ =>
      }
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

