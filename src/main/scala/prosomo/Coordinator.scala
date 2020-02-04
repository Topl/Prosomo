package prosomo

import java.io.{BufferedWriter, FileWriter}
import java.io.File
import java.time.Instant
import java.time.temporal.ChronoUnit

import akka.actor.{Actor, ActorRef, Props, Timers}
import bifrost.crypto.hash.FastCryptographicHash
import io.circe.Json
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.encode.Base58
import scala.math.BigInt
import scala.reflect.io.Path
import scala.util.{Random, Try}
import scala.sys.process._

/**
  * Coordinator actor that initializes the genesis block and instantiates the staking party,
  * sends messages to participants to execute a round
  */

class Coordinator extends Actor
  with Timers
  with Methods
  with CoordinatorVariables {
  val coordId = s"${self.path}"
  val sysLoad:SystemLoadMonitor = new SystemLoadMonitor
  var loadAverage = Array.fill(numAverageLoad){0.0}
  var genBlock:Block = _
  var roundDone = true

  private case object timerKey

  rng = new Random(BigInt(FastCryptographicHash(inputSeed+"coord")).toLong)

  def receive: Receive = {

      /**populates the holder list with stakeholder actor refs, the F_init functionality */
    case Populate => {
      println(s"Epoch Length = $epochLength")
      println(s"Delta = $delta_s")
      println("Populating")
      routerRef = context.actorOf(Router.props(FastCryptographicHash(inputSeed+"router")), "Router")
      var i = -1
      holders = List.fill(numHolders){
        i+=1
        context.actorOf(Stakeholder.props(FastCryptographicHash(inputSeed+i.toString)), "Holder:" + bytes2hex(FastCryptographicHash(inputSeed+i.toString)))
      }
      println("Sending holders list")
      sendAssertDone(List(routerRef),holders)
      sendAssertDone(holders,holders)
      println("Sending holders coordinator ref")
      sendAssertDone(holders,RouterRef(routerRef))
      sendAssertDone(holders,CoordRef(self))
      println("Getting holder keys")
      genKeys = collectKeys(holders,RequestKeys,genKeys)
      assert(!containsDuplicates(genKeys))
      println("Forge Genesis Block")
      genBlock = forgeGenBlock
      println("Send GenBlock")
      sendAssertDone(holders,GenBlock(genBlock))
      println("Send Router Keys")
      sendAssertDone(routerRef,holderKeys)
    }

      /**tells actors to print their inbox */
    case Inbox => sendAssertDone(holders,Inbox)

      /**sends start command to each stakeholder*/
    case Run => {
      println("Diffuse Holder Info")
      sendAssertDone(holders,Diffuse)
      println("Starting")
      sendAssertDone(holders,Initialize(L_s))
      if (useFencing) sendAssertDone(routerRef,CoordRef(self))
      println("Run")
      t0 = System.currentTimeMillis()
      sendAssertDone(holders,SetClock(t0))
      if (useFencing) sendAssertDone(routerRef,SetClock(t0))
      if (useFencing) routerRef ! Run
      for (holder<-rng.shuffle(holders)) {
        holder ! Run
      }
      if (!useFencing) timers.startPeriodicTimer(timerKey, ReadCommand, commandUpdateTime)
    }

      /**returns offset time to stakeholder that issues GetTime to coordinator*/
    case GetTime => {
      if (!actorStalled) {
        val t1 = System.currentTimeMillis()-tp
        sender() ! GetTime(t1)
      } else {
        sender() ! GetTime(tp)
      }
    }

      /**coordinator creates a file writer object that is passed to stakeholders if data is being written*/
    case NewDataFile => {
      if(dataOutFlag) {
        val dataPath = Path(dataFileDir)
        Try(dataPath.createDirectory())
        val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
        val uid = uuid
        fileWriter = new BufferedWriter(new FileWriter(s"$dataFileDir/ouroboros-data-$uid-$dateString.data"))
        val fileString = (
          "Holder_number"
            + " t"
            + " alpha"
            + " blocks_forged"
            + " chain_length"
            +" \n"
          )
        fileWriter match {
          case fw: BufferedWriter => {fw.write(fileString)}
          case _ => println("error: file writer not initialized")
        }
      }
    }

      /**passes fileWriter to actor who requests it with WriteFile*/
    case WriteFile => {
      sender() ! WriteFile(fileWriter)
    }

      /**closes the writer object to make sure data is written from buffer*/
    case CloseDataFile => if(dataOutFlag) {
      fileWriter match {
        case fw:BufferedWriter => fw.close()
        case _ => println("error: file writer close on non writer object")
      }
    }

    case NextSlot => {
      if (!actorPaused && !actorStalled) {
        if (roundDone) {
          t += 1
          roundDone = false
          routerRef ! NextSlot
        }
      }
    }

    case EndStep => {
      readCommand
      roundDone = true
    }

      /**command interpretation from config and cmd script*/
    case ReadCommand => {
      readCommand
    }

    case unknown:Any => if (!actorStalled) {
      print("received unknown message ")
      if (sender() == routerRef) {
        print("from router")
      }
      if (holders.contains(sender())) {
        print("from holder "+holders.indexOf(sender()).toString)
      }
      println(": "+unknown.getClass.toString+" "+unknown.toString)
    }
  }

  /**randomly picks two holders and creates a transaction between the two*/
  def issueRandTx = {
    for (i <- 0 to txProbability.floor.toInt) {
      val holder1 = rng.shuffle(holders).head
      val r = rng.nextDouble
      if (r<txProbability%1.0) {
        val holder2 = holders.filter(_ != holder1)(rng.nextInt(holders.length-1))
        assert(holder1 != holder2)
        val delta:BigInt = BigDecimal(maxTransfer*rng.nextDouble).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        holder1 ! IssueTx((holderKeys(holder2),delta))
        transactionCounter += 1
      }
    }
  }

  def issueTx(holder1:ActorRef,holder2:ActorRef,delta:BigInt) = {
    holder1 ! IssueTx((holderKeys(holder2),delta))
    transactionCounter += 1
  }

  /**command string interpreter*/
  def command(sl:List[String]): Unit = {
    for (s<-sl.reverse){
      s.trim match {

        case "status_all" => {
          sharedData.txCounter = 0
          sharedData.setOfTxs = Map()
          sendAssertDone(holders,Status)
          assert(sharedData.setOfTxs.keySet.size == sharedData.txCounter)
          println("Total Transactions: "+sharedData.setOfTxs.keySet.size.toString)
          println("Total Attempts to Issue Txs:"+transactionCounter.toString)
          sharedData.txCounter = 0
        }

        case "fence_step" => sendAssertDone(routerRef,"fence_step")

        case "verify_all" => sendAssertDone(holders,Verify)

        case "stall" => sendAssertDone(holders,StallActor)

        case "pause" => {
          if (!actorPaused) {
            actorPaused = true
            if (!actorStalled) {
              actorStalled = true
              tp = System.currentTimeMillis()-tp
            }
          } else {
            actorPaused = false
            if (actorStalled) {
              actorStalled = false
              tp = System.currentTimeMillis()-tp
            }
          }
        }

        case "inbox" => sendAssertDone(holders,Inbox)

        case "randtx" => if (!transactionFlag) {transactionFlag = true} else {transactionFlag = false}

        case "write" => fileWriter match {
          case fw:BufferedWriter => fw.flush()
          case _ => println("File writer not initialized")
        }

        case "graph" => {
          println("Writing network graph matrix...")
          gossipersMap = getGossipers(holders)
          val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
          val uid = uuid
          graphWriter = new BufferedWriter(new FileWriter(s"$dataFileDir/ouroboros-graph-$uid-$dateString.graph"))
          graphWriter match {
            case fw:BufferedWriter => {
              var line:String = ""
              for (holder<-holders) {
                line = ""
                for (ref<-holders) {
                  if (gossipersMap(holder).contains(ref)) {
                    line = line + "1"
                  } else {
                    line = line + "0"
                  }
                  if (holders.indexOf(ref)!=holders.length-1) {
                    line = line + " "
                  }
                }
                fw.write(line+"\n")
              }
              fw.flush()
            }
            case _ =>
          }
          graphWriter match {
            case fw:BufferedWriter => {
              fw.close()
            }
            case _ =>
          }
        }

        case "tree_all" => {
          for (holder<-holders) {
            printTree(holder)
          }
        }

        case "tree" => {
         printTree(holders(sharedData.printingHolder))
        }

        case "kill" => {
          sharedData.killFlag = true
          timers.cancelAll
          fileWriter match {
            case fw:BufferedWriter => fw.close()
            case _ => println("error: file writer close on non writer object")
          }
          Thread.sleep(2*slotT*delta_s)
          context.system.terminate
        }

        case "split" => {
          parties = List()
          val (holders1,holders2) = rng.shuffle(holders).splitAt(rng.nextInt(holders.length-2)+1)
          println("Splitting Party into groups of "+holders1.length.toString+" and "+holders2.length.toString)
          sendAssertDone(holders1,Party(holders1,true))
          sendAssertDone(holders1,Diffuse)
          sendAssertDone(holders2,Party(holders2,true))
          sendAssertDone(holders2,Diffuse)
          parties ::= holders1
          parties ::= holders2
          gossipersMap = getGossipers(holders)
        }

        case "bridge" => {
          parties = List()
          val (holders1,holders2) = rng.shuffle(holders).splitAt(rng.nextInt(holders.length-3)+2)
          println("Bridging Party into groups of "+holders1.length.toString+" and "+holders2.length.toString)
          val commonRef = holders1.head
          sendAssertDone(holders,Party(List(),true))
          sendAssertDone(List(commonRef),Party(holders,false))
          sendAssertDone(List(commonRef),Diffuse)
          sendAssertDone(holders1.tail,Party(holders1,false))
          sendAssertDone(holders1.tail,Diffuse)
          sendAssertDone(holders2,Party(commonRef::holders2,false))
          sendAssertDone(holders2,Diffuse)
          parties ::= holders1
          parties ::= holders2
          gossipersMap = getGossipers(holders)
        }

        case "join" => {
          parties = List()
          println("Joining Parties")
          sendAssertDone(holders,Party(holders,true))
          sendAssertDone(holders,Diffuse)
          parties ::= holders
          gossipersMap = getGossipers(holders)
        }

        case "new_holder" => {
          println("Bootstrapping new holder...")
          val i = holders.length
          val newHolder = context.actorOf(Stakeholder.props(FastCryptographicHash(inputSeed+i.toString)), "Holder:" + bytes2hex(FastCryptographicHash(inputSeed+i.toString)))
          holders = holders++List(newHolder)
          sendAssertDone(routerRef,holders)
          sendAssertDone(newHolder,holders)
          sendAssertDone(newHolder,RouterRef(routerRef))
          sendAssertDone(newHolder,CoordRef(self))
          sendAssertDone(newHolder,GenBlock(genBlock))
          val newHolderKeys = collectKeys(List(newHolder),RequestKeys,Map())
          val newHolderKeyW = ByteArrayWrapper(hex2bytes(newHolderKeys(s"${newHolder.path}").split(";")(0))++hex2bytes(newHolderKeys(s"${newHolder.path}").split(";")(1))++hex2bytes(newHolderKeys(s"${newHolder.path}").split(";")(2)))
          holderKeys += (newHolder-> newHolderKeyW)
          sendAssertDone(holders,Party(holders,true))
          sendAssertDone(holders,Diffuse)
          sendAssertDone(newHolder,Initialize(L_s))
          sendAssertDone(newHolder,SetClock(t0))
          newHolder ! Run
        }

        case value:String => {
          val arg0 = "print_"
          if (value.slice(0,arg0.length) == arg0) {
            val index:Int = value.drop(arg0.length).toInt
            sharedData.printingHolder = index
          }

          val arg1 = "stall_"
          if(value.slice(0,arg1.length) == arg1) {
            val index:Int = value.drop(arg1.length).toInt
            if (index < holders.length) {
              sendAssertDone(holders(index),StallActor)
              println(s"Holder $index sent stall signal")
            }
          }

          val arg2 = "split_stake_"
          if(value.slice(0,arg2.length) == arg2) {
            val ratio:Double =  value.drop(arg2.length).toDouble
            assert(ratio>0.0 && ratio<1.0)
            val stakingState:State = getStakingState(holders.head)
            val netStake:BigInt = {
              var out:BigInt = 0
              for (holder<-holders){
                out += stakingState(holderKeys(holder))._1
              }
              out
            }

            var holders1:List[ActorRef] = List()
            var net1:BigInt = 0
            var holders2:List[ActorRef] = List()
            var net2:BigInt = 0
            for (holder <- rng.shuffle(holders)) {
              val holderStake = stakingState(holderKeys(holder))._1
              if (net1< BigDecimal(ratio*(net1.toDouble+net2.toDouble)).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt) {
                net1 += holderStake
                holders1 ::= holder
              } else {
                net2 += holderStake
                holders2 ::= holder
              }
            }
            val alpha1 = net1.toDouble/netStake.toDouble
            val alpha2 = net2.toDouble/netStake.toDouble
            val numh1 = holders1.length
            val numh2 = holders2.length

            parties = List()

            println(s"Splitting Stake to $alpha1 and $alpha2 with $numh1 and $numh2 holders")
            sendAssertDone(holders1,Party(holders1,true))
            sendAssertDone(holders1,Diffuse)
            sendAssertDone(holders2,Party(holders2,true))
            sendAssertDone(holders2,Diffuse)
            parties ::= holders1
            parties ::= holders2
            gossipersMap = getGossipers(holders)
          }

          val arg3 = "bridge_stake_"
          if (value.slice(0,arg3.length) == arg3) {
            val ratio:Double =  value.drop(arg3.length).toDouble
            assert(ratio>0.0 && ratio<1.0)
            val stakingState:State = getStakingState(holders.head)
            val netStake:BigInt = {
              var out:BigInt = 0
              for (holder<-holders){
                out += stakingState(holderKeys(holder))._1
              }
              out
            }
            var holders1:List[ActorRef] = List()
            var net1:BigInt = 0
            var holders2:List[ActorRef] = List()
            var net2:BigInt = 0
            for (holder <- rng.shuffle(holders)) {
              val holderStake = stakingState(holderKeys(holder))._1
              if (net1<BigDecimal(ratio*(net1.toDouble+net2.toDouble)).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt) {
                net1 += holderStake
                holders1 ::= holder
              } else {
                net2 += holderStake
                holders2 ::= holder
              }
            }
            val alpha1 = net1.toDouble/netStake.toDouble
            val alpha2 = net2.toDouble/netStake.toDouble
            val numh1 = holders1.length
            val numh2 = holders2.length

            parties = List()

            println(s"Bridging Stake to $alpha1 and $alpha2 with $numh1 and $numh2 holders")
            val commonRef = holders1.head
            sendAssertDone(holders,Party(List(),true))
            sendAssertDone(List(commonRef),Party(holders,false))
            sendAssertDone(List(commonRef),Diffuse)
            sendAssertDone(holders1.tail,Party(holders1,false))
            sendAssertDone(holders1.tail,Diffuse)
            sendAssertDone(holders2,Party(commonRef::holders2,false))
            sendAssertDone(holders2,Diffuse)
            parties ::= holders1
            parties ::= holders2
            gossipersMap = getGossipers(holders)
          }

          val arg4 = "issue_"
          if (value.slice(0,arg4.length) == arg4) {
            val data:String = value.drop(arg4.length)
            val hIndex1s:String = data.split("_")(0)
            val hIndex2s:String = data.split("_")(1)
            val deltas:String = data.split("_")(2)
            val holder1:ActorRef = holders(hIndex1s.toInt)
            val holder2:ActorRef = holders(hIndex2s.toInt)
            val delta:BigInt = BigInt(deltas)
            issueTx(holder1,holder2,delta)
          }

          val arg5 = "balance_"
          if (value.slice(0,arg5.length) == arg5) {
            val data = value.drop(arg5.length)
            holders(data.toInt) ! GetBalance
          }

          val arg6 = "status_"
          if (value.slice(0,arg6.length) == arg6) {
            val data = value.drop(arg6.length)
            sendAssertDone(holders(data.toInt),Status)
            println("Total Txs:"+transactionCounter.toString)
          }

          val arg7 = "verify_"
          if (value.slice(0,arg7.length) == arg7) {
            val data = value.drop(arg7.length)
            sendAssertDone(holders(data.toInt),Verify)
          }

          val arg8 = "adversary_"
          if (value.slice(0,arg8.length) == arg8) {
            val data = value.drop(arg8.length)
            sendAssertDone(holders(data.toInt),Adversary(""))
          }

          val arg9 = "covert_"
          if (value.slice(0,arg9.length) == arg9) {
            val data = value.drop(arg9.length)
            sendAssertDone(holders(data.toInt),Adversary("covert"))
          }

          val arg10 = "nas_"
          if(value.slice(0,arg10.length) == arg10) {
            val data = value.drop(arg10.length)
            sendAssertDone(holders(data.toInt),Adversary("nas"))
          }

        }
        case _ =>
      }
    }
  }

  def readCommand = {
    if (!useFencing) {
      if (!actorStalled) {
        val t1 = System.currentTimeMillis()-tp
        t = ((t1 - t0) / slotT).toInt
      } else {
        t = ((tp - t0) / slotT).toInt
      }
    }
    if (new File("/tmp/scorex/test-data/crypto/cmd").exists) {
      println("-----------------------------------------------------------")
      val f = new File("/tmp/scorex/test-data/crypto/cmd")
      val cmd: String = ("cat" #< f).!!
      f.delete
      val cmdList = cmd.split("\n")
      for (line<-cmdList) {
        val com = line.trim.split(" ")
        com(0) match {
          case s:String => {
            if (com.length == 2){
              com(1).toInt match {
                case i:Int => {
                  if (cmdQueue.keySet.contains(i)) {
                    val nl = s::cmdQueue(i)
                    cmdQueue -= i
                    cmdQueue += (i->nl)
                  } else {
                    cmdQueue += (i->List(s))
                  }
                }
                case _ =>
              }
            } else {
              if (cmdQueue.keySet.contains(t)){
                val nl = s::cmdQueue(t)
                cmdQueue -= t
                cmdQueue += (t->nl)
              } else {
                cmdQueue += (t->List(s))
              }
            }
          }
          case _ =>
        }
      }
    }
    if (cmdQueue.keySet.contains(t)) {
      command(cmdQueue(t))
      cmdQueue -= t
    }
    if (performanceFlag && !useFencing) {
      val newLoad = sysLoad.cpuLoad
      if (newLoad>0.0){
        loadAverage = loadAverage.tail++Array(newLoad)
      }

      if (!actorPaused) {
        val cpuLoad = (0.0 /: loadAverage){_ + _}/loadAverage.length
        if (cpuLoad >= systemLoadThreshold && !actorStalled) {
          tp = System.currentTimeMillis()-tp
          actorStalled = true
        } else if (cpuLoad < systemLoadThreshold && actorStalled) {
          tp = System.currentTimeMillis()-tp
          actorStalled = false
        }
      }
    }

    if (!actorStalled && transactionFlag && !useFencing && t>1 && t<L_s && transactionCounter < txMax && !sharedData.errorFlag) {
      issueRandTx
    }

    if (sharedData.killFlag || t>L_s+2*delta_s) {
      timers.cancelAll
      fileWriter match {
        case fw:BufferedWriter => fw.close()
        case _ => println("error: file writer close on non writer object")
      }
      context.system.terminate
    }
  }

  def printTree(holder:ActorRef):Unit = {
    var tn = 0
    if (useFencing) {
      tn = t
    } else {
      if (!actorStalled) {
        val t1 = System.currentTimeMillis()-tp
        tn = ((t1 - t0) / slotT).toInt
      } else {
        val t1 = tp
        tn = ((t1 - t0) / slotT).toInt
      }
    }
    getBlockTree(holder)
    val positionData:(Map[ActorRef,(Double,Double)],Map[(ActorRef,ActorRef),Long]) = getPositionData(routerRef)
    val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
    val uid = uuid
    val holderIndex = holders.indexOf(holder)
    graphWriter = new BufferedWriter(new FileWriter(s"$dataFileDir/ouroboros-holder-$holderIndex-$uid-$dateString.tree"))
    val configString = {
      import Prosomo.input
      if (input.length > 0) { input.head.stripSuffix(".conf")+".conf"} else {""}
    }
    graphWriter match {
      case fw:BufferedWriter => {
        val json:Json = Map(
          "info" -> Map(
            "config"-> configString.asJson,
            "numHolders"-> numHolders.asJson,
            "slotT" -> slotT.asJson,
            "delay_ms_km" -> delay_ms_km.asJson,
            "useRouting" -> useRouting.asJson,
            "delta_s" -> delta_s.asJson,
            "useDelayParam" -> useDelayParam.asJson,
            "k_s" -> k_s.asJson,
            "f_s" -> f_s.asJson,
            "L_s" -> L_s.asJson,
            "epochLength" -> epochLength.asJson,
            "slotWindow" -> slotWindow.asJson,
            "confirmationDepth" -> confirmationDepth.asJson,
            "initStakeMax" -> initStakeMax.asJson,
            "maxTransfer" -> maxTransfer.asJson,
            "forgerReward" -> forgerReward.asJson,
            "transactionFee" -> transactionFee.asJson,
            "numGossipers" -> numGossipers.asJson,
            "useGossipProtocol" -> useGossipProtocol.asJson,
            "tineMaxTries" -> tineMaxTries.asJson,
            "tineMaxDepth" -> tineMaxDepth.asJson,
            "dataOutInterval" -> dataOutInterval.asJson,
            "waitTime" -> waitTime.toMillis.asJson,
            "updateTime" -> updateTime.toMillis.asJson,
            "commandUpdateTime" -> commandUpdateTime.toMillis.asJson,
            "transactionFlag" -> transactionFlag.asJson,
            "txDenominator" -> txProbability.asJson,
            "randomFlag" -> randomFlag.asJson,
            "performanceFlag" -> performanceFlag.asJson,
            "systemLoadThreshold" -> systemLoadThreshold.asJson,
            "numAverageLoad" -> numAverageLoad.asJson,
            "printFlag" -> printFlag.asJson,
            "timingFlag" -> timingFlag.asJson,
            "dataOutFlag" -> dataOutFlag.asJson,
            "dataFileDir" -> dataFileDir.asJson,
            "useFencing" -> useFencing.asJson,
            "inputSeed" -> inputSeed.asJson
          ).asJson,
          "position" -> Map(
            "delay" -> positionData._2.map{
              case value: ((ActorRef,ActorRef),Long) => {
                Array(holders.indexOf(value._1._1).asJson,holders.indexOf(value._1._2).asJson,value._2.asJson)
              }
            }.asJson,
            "coordinates" -> positionData._1.map{
              case value:(ActorRef,(Double,Double)) => {
                Map(holders.indexOf(value._1).toString -> Array(value._2._1.asJson,value._2._2.asJson)).asJson
              }
            }.asJson
          ).asJson,
          "data" -> (0 to tn).toArray.map{
            case i:Int => Map(
              "slot" -> i.asJson,
              "blocks" -> blocks(i).map{
                case value:(ByteArrayWrapper,Block) => {
                  val (pid:Hash,ledger:Ledger,bs:Slot,cert:Cert,vrfNonce:Rho,noncePi:Pi,kesSig:KesSignature,pk_kes:PublicKey,bn:Int,ps:Slot) = value._2
                  val (pk_vrf:PublicKey,y:Rho,ypi:Pi,pk_sig:PublicKey,thr:Double,info:String) = cert
                  val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
                  Map(
                    "id" -> Base58.encode(value._1.data).asJson,
                    "bn" -> bn.asJson,
                    "bs" -> bs.asJson,
                    "pid" -> Base58.encode(pid.data).asJson,
                    "ps" -> ps.asJson,
                    "nonce" -> Base58.encode(vrfNonce).asJson,
                    "npi" -> Base58.encode(noncePi).asJson,
                    "y" -> Base58.encode(y).asJson,
                    "ypi" -> Base58.encode(ypi).asJson,
                    "thr" -> thr.asJson,
                    "info" -> info.asJson,
                    "sig" -> Array(Base58.encode(kesSig._1).asJson,Base58.encode(kesSig._2).asJson,Base58.encode(kesSig._3).asJson).asJson,
                    "ledger" -> ledger.toArray.map{
                      case box:Box => {
                        box._1 match {
                          case entry:(ByteArrayWrapper,PublicKeyW,BigInt) => {
                            val delta = entry._3
                            val pk_g:PublicKeyW = entry._2
                            Map(
                              "genesis" -> Base58.encode(pk_g.data).asJson,
                              "amount" -> delta.toLong.asJson
                            ).asJson
                          }
                          case entry:(ByteArrayWrapper,BigInt) => {
                            val delta = entry._2
                            Map(
                              "forger" -> Base58.encode(pk_f.data).asJson,
                              "amount" -> delta.toLong.asJson
                            ).asJson
                          }
                        }
                      }
                      case trans:Transaction => {
                        Map(
                          "txid" -> Base58.encode(trans._4.data).asJson,
                          "count" -> trans._5.asJson,
                          "sender" -> Base58.encode(trans._1.data).asJson,
                          "recipient" -> Base58.encode(trans._2.data).asJson,
                          "amount" -> trans._3.toLong.asJson
                        ).asJson
                      }
                    }.asJson
                  ).asJson
                }
              }.asJson,
              "history" -> chainHistory(i).map{
                case value:BlockId => Map(
                  "id" -> Base58.encode(value._2.data).asJson
                ).asJson
              }.asJson
            ).asJson
          }.asJson
        ).asJson
        fw.write(json.toString)
        fw.flush()
      }
      case _ =>
    }
    graphWriter match {
      case fw:BufferedWriter => {
        fw.close()
      }
      case _ =>
    }
  }

  /**creates genesis block to be sent to all stakeholders */
  def forgeGenBlock: Block = {
    val bn:Int = 0
    val ps:Slot = -1
    val slot:Slot = 0
    val pi:Pi = vrf.vrfProof(sk_vrf,eta0++serialize(slot)++serialize("NONCE"))
    val rho:Rho = vrf.vrfProofToHash(pi)
    val pi_y:Pi = vrf.vrfProof(sk_vrf,eta0++serialize(slot)++serialize("TEST"))
    val y:Rho = vrf.vrfProofToHash(pi_y)
    val h:Hash = ByteArrayWrapper(eta0)
    val ledger: Ledger = holders.map{
      case ref:ActorRef => {
        val initStake = {
          val out = stakeDistribution match {
            case "random" => {initStakeMax*rng.nextDouble}
            case "exp" => {initStakeMax*math.exp(-stakeScale*holders.indexOf(ref).toDouble)}
            case "flat" => {initStakeMax}
          }
          if (initStakeMax > initStakeMin && out > initStakeMin) {
            out
          } else {
            if (initStakeMin > 1.0) {
              initStakeMin
            } else {
              1.0
            }
          }
        }
        val pkw = ByteArrayWrapper(hex2bytes(genKeys(s"${ref.path}").split(";")(0))++hex2bytes(genKeys(s"${ref.path}").split(";")(1))++hex2bytes(genKeys(s"${ref.path}").split(";")(2)))
        holderKeys += (ref-> pkw)
        signBox((genesisBytes, pkw, BigDecimal(initStake).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt), ByteArrayWrapper(FastCryptographicHash(coordId)),sk_sig,pk_sig)
      }
    }
    val cert:Cert = (pk_vrf,y,pi_y,pk_sig,1.0,"")
    val sig:KesSignature = kes.sign(sk_kes, h.data++serialize(ledger)++serialize(slot)++serialize(cert)++rho++pi++serialize(bn)++serialize(ps))
    (h,ledger,slot,cert,rho,pi,sig,pk_kes,bn,ps)
  }
}

object Coordinator {
  def props: Props = Props(new Coordinator)
}
