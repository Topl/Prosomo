package prosomo.stakeholder

import akka.actor.{Actor, ActorPath, Props, Timers}
import akka.routing.{ActorRefRoutee,SmallestMailboxRoutingLogic,Router}
import com.google.common.primitives.Bytes
import akka.util.Timeout
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases._
import prosomo.components.Serializer
import prosomo.primitives.{Distance, Ecx, Fch, Mac, Parameters, SharedData, Types}
import prosomo.remote.{DiffuseDataSpec, _}
import scorex.util.encode.Base58

import scala.collection.immutable.ListMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.math.BigInt
import scala.util.{Random, Success, Try}
import scorex.core.network._
import scorex.core.network.message.{Message, MessageSpec}
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.NetworkController.ReceivableMessages.{RegisterMessageSpecs, SendToNetwork}

import prosomo.remote.SpecTypes.{
  DiffuseDataType,
  HelloDataType,
  HoldersType,
  RequestBlockType,
  RequestTineType,
  ReturnBlocksType,
  SendBlockType,
  SendTxType
}

/**
  * AMS 2020:
  * Router Actor
  * Primary interface between Stakeholder system and network controller,
  * All messages from local and remote are processed here or in a set of child actors handled by the registered actor,
  * Starts a system of routees for ingress and egress traffic
  * A separate local only instance calculates simulation delay messages for local to local comms
  * Acts as remote interface, should only communicate with Stakeholder and Network Controller,
  * Ingress messages are authenticated with Ecx X25519 Hash Mac
  * F^Delta^_{N-MC} multicast network functionality
  * @param seed entropy for randomness
  * @param inputRef network controller refs
  */

class RouteProcessor(seed:Array[Byte], inputRef:Seq[ActorRefWrapper]) extends Actor
  with Types
  with Timers {
  import Parameters._
  val networkController:ActorRefWrapper = inputRef.head
  val peerManager:ActorRefWrapper = inputRef(1)
  implicit val routerRef: ActorRefWrapper = {
    Try{inputRef(2)}.toOption match {
      case Some(ref:ActorRefWrapper) => ref
      case None => ActorRefWrapper.routerRef(self)
    }
  }
  var coordinatorRef:ActorRefWrapper = _
  Try{inputRef(3)}.toOption match {
    case Some(ref:ActorRefWrapper) => coordinatorRef = ref
    case None =>
  }

  val serializer:Serializer = new Serializer
  var holders:List[ActorRefWrapper] = List()
  val rng = new Random(BigInt(seed).toLong)
  val fch = new Fch
  val ecx = new Ecx
  var holdersPosition:Map[ActorRefWrapper,(Double,Double)] = Map()
  var distanceMap:Map[(ActorRefWrapper,ActorRefWrapper),Long] = Map()
  var holderMessages:Map[Slot,Map[Long,Map[ActorRefWrapper,Map[BigInt,(ActorRefWrapper,ActorRefWrapper,Any)]]]] = Map()
  var holderReady:Map[ActorRefWrapper,Boolean] = Map()
  var globalSlot:Slot = 0
  var localSlot:Slot = 0
  var t0:Long = 0
  var ts:Long = 0
  var roundDone = true
  var firstDataPass = true
  var roundStep = "updateSlot"
  val printSteps = false
  var txRoundCounter = 0
  var maxDelay:Double = 0
  var transactionCounter:Int = 0

  var pathToPeer:Map[ActorPath,(String,PublicKey,Long)] = Map()
  var bootStrapJobs:Set[ActorRefWrapper] = Set()

  var router:Option[Router] = None

  case class RouterPeerInfo(pathToPeer:Map[ActorPath,(String,PublicKey,Long)],
                            bootStrapJobs:Set[ActorRefWrapper],
                            holders:List[ActorRefWrapper])

  var systemTime:Long = System.nanoTime()
  var messageTime:Long = 0

  private case object ActorPathSendTimerKey

  /**
    * Message time for MAC authentication,
    * Use this instead of nanoTime so the time increments
    * properly if successive calls to nanoTime return the same value
    * @return next message time
    */

  def nextMsgTime():Long = {
    System.nanoTime() match {
      case newTime:Long if newTime > systemTime =>
        systemTime = newTime
        messageTime = 0
        systemTime
      case _ =>
        messageTime += 1
        systemTime + messageTime
    }
  }

  /**
    * Sends commands one by one to list of stakeholders
    * @param holders actor list
    * @param command object to be sent
    */
  def sendAssertDone(holders:List[ActorRefWrapper], command: Any): Unit = {
    for (holder <- holders){
      implicit val timeout:Timeout = Timeout(waitTime)
      val future = holder ? command
      val result = Await.result(future, timeout.duration)
      assert(result == "done")
    }
  }

  /**
    * Sends command to stakeholder and waits for response
    * @param holder to send to
    * @param command any command
    */
  def sendAssertDone(holder:ActorRefWrapper, command: Any): Unit = {
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = holder ? command
    val result = Await.result(future, timeout.duration)
    assert(result == "done")
  }

  def holdersReady:Boolean = {
    var bool = true
    for (holder <- holders.filterNot(_.remote)){
      bool &&= holderReady(holder)
    }
    bool
  }

  def reset():Unit = {
    for (holder <- holders.filterNot(_.remote)){
      if (holderReady.keySet.contains(holder)) holderReady -= holder
      holderReady += (holder->false)
    }
  }

  def reset(holder:ActorRefWrapper):Unit = {
    if (holderReady.keySet.contains(holder)) holderReady -= holder
    holderReady += (holder->false)
  }

  def delay(sender:ActorRefWrapper, recip:ActorRefWrapper, data:Any):FiniteDuration = {
    if (!distanceMap.keySet.contains((sender,recip))) {
      distanceMap += ((sender,recip)->(delay_ms_km*1.0e6*Distance.calculate(
        holdersPosition(sender)._1,
        holdersPosition(sender)._2,
        holdersPosition(recip)._1,
        holdersPosition(recip)._2,
        "K")).toLong)
    }
    val delay_ns:Long = (rng.nextDouble()*delay_ms_noise*1.0e6).toLong
      + (serializer.getAnyBytes(data).length*delay_ms_byte*1.0e6).toLong + distanceMap((sender,recip))
    if (delay_ns/1.0e9 > maxDelay) {maxDelay = delay_ns/1.0e9}
    delay_ns.nano
  }

  def deliver(): Unit = {
    var slotMessages = holderMessages(globalSlot)
    val next_message_t = slotMessages.keySet.min
    if (next_message_t > (txRoundCounter*commandUpdateTime.toNanos)) {
      txRoundCounter += 1
      ts = txRoundCounter*commandUpdateTime.toNanos
      issueTx()
    } else {
      holderMessages -= globalSlot
      ts = next_message_t
      var queue:Map[ActorRefWrapper,Map[BigInt,(ActorRefWrapper,ActorRefWrapper,Any)]] = slotMessages(ts)
      slotMessages -= ts
      for (holder <- rng.shuffle(holders.filterNot(_.remote))) {
        if (queue.keySet.contains(holder)) {
          var messageMap:Map[BigInt,(ActorRefWrapper,ActorRefWrapper,Any)] = queue(holder)
          queue -= holder
          val message = ListMap(messageMap.toSeq.sortBy(_._1):_*).head
          messageMap -= message._1
          val (s,r,c) = message._2
          reset(r)
          if (printSteps) println(
            holders.indexOf(s),
            holders.indexOf(r),
            c.getClass,message._1,
            c match {
              case value:SendBlock => Base58.encode(value.block.id.data)
              case value:SendTx => Base58.encode(value.transaction.sid.data)
              case _ => " "
            }
          )
          if (!r.remote)
            context.system.scheduler.scheduleOnce(0.nano,r.actorRef,c)(context.system.dispatcher,s.actorRef)
          if (messageMap.nonEmpty) queue += (holder->messageMap)
        }
      }
      if (queue.nonEmpty) slotMessages += (ts->queue)
      if (slotMessages.nonEmpty) holderMessages += (globalSlot -> slotMessages)
    }
  }

  /**randomly picks two holders and creates a transaction between the two*/
  def issueTx(): Unit = {
    for (_ <- 0 to txProbability.floor.toInt) {
      val holder1 = rng.shuffle(holders.filterNot(_.remote)).head
      val r = rng.nextDouble
      if (r<txProbability%1.0) {
        val holder2 = holders.filter(_ != holder1)(rng.nextInt(holders.length-1))
        assert(holder1 != holder2)
        val delta:BigInt = BigDecimal(maxTransfer*rng.nextDouble).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        reset(holder1)
        transactionCounter += 1
        context.system.scheduler.scheduleOnce(0.nano,holder1.actorRef,
          IssueTx(holder2,delta)
        )(context.system.dispatcher,self)
      }
    }
  }

  def update: Any = {
    if (SharedData.killFlag) {
      timers.cancelAll
      context.system.terminate
    } else {
      if (roundDone) {
        coordinatorRef ! NextSlot
      }
      if (globalSlot > localSlot && !roundDone) {
        assert(globalSlot == localSlot + 1)
        localSlot = globalSlot
        ts = 0
        roundStep = "updateSlot"
        if (printSteps) println("--------start----------")
        reset()
        sendAssertDone(holders.filterNot(_.remote),GetSlot(globalSlot))
      } else {
        roundStep match {
          case "updateSlot" =>
            if (holdersReady) {
              roundStep = "passData"
              reset()
            }
          case "passData" =>
            if (holdersReady) {
              if (holderMessages.keySet.contains(globalSlot)) {
                if (printSteps) println("-------deliver---------")
                deliver()
              } else {
                if (slotT*1000000>(txRoundCounter*commandUpdateTime.toNanos)) {
                  txRoundCounter += 1
                  ts = txRoundCounter*commandUpdateTime.toNanos
                  issueTx()
                } else {
                  roundStep = "endStep"
                  if (printSteps) println("---------end-----------")
                  reset()
                  for (holder<-holders.filterNot(_.remote)) {
                    holder ! "endStep"
                  }
                }
              }
            } else {
              if (firstDataPass) {
                if (printSteps) println("---------first-----------")
                for (holder<-holders.filterNot(_.remote)) {
                  holder ! "passData"
                }
                firstDataPass = false
              }
            }
          case "endStep" => if (holdersReady && !roundDone) {
            if (printSteps) println("--------reset----------")
            roundDone = true
            firstDataPass = true
            txRoundCounter = 0
            coordinatorRef ! EndStep
          }
          case _ =>
        }
      }
    }
  }

  def routerReceive: Receive = {

    case Flag(ref,value) =>
      if (value == roundStep && holderReady.keySet.contains(ref)) {
        holderReady -= ref
        holderReady += (ref -> true)
      }

    case NextSlot =>
      if (roundDone) globalSlot += 1
      roundDone = false

    case MessageFromLocalToLocalId(uid,s,r,c) =>
      val newMessage = (s,r,c)
      val nsDelay = delay(s,r,c)
      val messageDelta:Slot = ((nsDelay.toNanos+ts)/(slotT*1000000)).toInt
      val priority:Long = (nsDelay.toNanos+ts)%(slotT*1000000)
      val offsetSlot = globalSlot+messageDelta
      val messages:Map[Long,Map[ActorRefWrapper,Map[BigInt,(ActorRefWrapper,ActorRefWrapper,Any)]]] =
        if (holderMessages.keySet.contains(offsetSlot)) {
        var m = holderMessages(offsetSlot)
        holderMessages -= offsetSlot
        if (m.keySet.contains(priority)) {
          var l = m(priority)
          m -= priority
          if (l.keySet.contains(s)) {
            var q = l(s)
            l -= s
            q += (uid -> newMessage)
            l += (s -> q)
          } else {
            l += (s -> Map(uid->newMessage))
          }
          m += (priority -> l)
          m
        } else {
          m += (priority -> Map(s -> Map(uid -> newMessage)))
          m
        }
      } else {
        Map(priority -> Map(s -> Map(uid -> newMessage)))
      }
      holderMessages += (offsetSlot-> messages)

    case Run =>
      timers.startPeriodicTimer(ActorPathSendTimerKey, Update, 1.nano)
      coordinatorRef ! NextSlot

    case value:CoordRef =>
      coordinatorRef = value.ref
      sender() ! "done"

    case value:String => if (value == "fence_step") {
      println(roundStep)
      sender() ! "done"
    }

    case Update => update

    case ActorPathSendTimerKey =>
      if (!holders.forall(_.remote)) holdersToNetwork()

    case value:SetClock =>
      t0 = value.t0
      sender() ! "done"

    case value:GetTime =>
      globalSlot = ((value.t1 - t0) / slotT).toInt

    case RequestPositionData =>
      sender() ! GetPositionData((holdersPosition,distanceMap))

  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val outTime = (t1 - t0)*1.0e-9
    val tString = "%6.6f".format(outTime)
    println("Elapsed time: " + tString + " s")
    result
  }

  def getRefs(sender:String,recipient:String):Option[(ActorRefWrapper,ActorRefWrapper)] = {
    Try{ActorPath.fromString(sender)} match {
      case Success(snd:ActorPath) =>
        Try{ActorPath.fromString(recipient)} match {
          case Success(rec:ActorPath) =>
            holders.find(_.actorPath == snd) match {
              case Some(out1:ActorRefWrapper) =>
                holders.find(_.actorPath == rec) match {
                  case Some(out2: ActorRefWrapper) => Some((out1,out2))
                  case None => None
                }
              case None => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  private def messageFromPeer: Receive = {
    case DataFromPeer(spec, data, remote) =>
      router match {
        case Some(rt) if spec.messageCode != HoldersFromRemoteSpec.messageCode => rt.route(DataFromPeer(spec, data, remote), sender)
        case _ =>
          spec.messageCode match {
            case HoldersFromRemoteSpec.messageCode =>
              data match {
                case msg:HoldersType@unchecked => Try{for (string<-msg._1) {
                  Try{ActorPath.fromString(string)}.toOption match {
                    case Some(newPath:ActorPath) =>
                      holders.find(_.path == newPath) match {
                        case None =>
                          holders ::= ActorRefWrapper(newPath)
                          pathToPeer += (newPath -> (remote.peerInfo.get.peerSpec.agentName,msg._2,msg._3))
                          SharedData.guiPeerInfo.get(remote.peerInfo.get.peerSpec.agentName) match {
                            case Some(list:List[ActorRefWrapper]) =>
                              val newList = ActorRefWrapper(newPath)::list
                              SharedData.guiPeerInfo -= remote.peerInfo.get.peerSpec.agentName
                              SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> newList)
                            case None =>
                              SharedData.guiPeerInfo +=
                                (remote.peerInfo.get.peerSpec.agentName -> List(ActorRefWrapper(newPath)))
                          }
                          println("New holder "+newPath.toString)
                          updatePeerInfo()
                          coordinatorRef ! HoldersFromRemote(holders)
                          if (!holders.forall(_.remote)) holdersToNetwork()
                        case Some(actorRef:ActorRefWrapper) =>
                          if (pathToPeer(actorRef.path)._1 != remote.peerInfo.get.peerSpec.agentName) {
                            if (SharedData.guiPeerInfo.keySet.contains(pathToPeer(actorRef.path)._1))
                              SharedData.guiPeerInfo -= pathToPeer(actorRef.path)._1
                            val key = actorRef.path
                            pathToPeer -= key
                            pathToPeer += (key -> (remote.peerInfo.get.peerSpec.agentName,msg._2,msg._3))
                            SharedData.guiPeerInfo.get(remote.peerInfo.get.peerSpec.agentName) match {
                              case Some(list:List[ActorRefWrapper]) =>
                                val newList = actorRef::list
                                SharedData.guiPeerInfo -= remote.peerInfo.get.peerSpec.agentName
                                SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> newList)
                              case None =>
                                SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> List(actorRef))
                            }
                            if (!holders.forall(_.remote)) holdersToNetwork()
                            println("Updated Peer "+newPath.toString)
                            updatePeerInfo()
                          }
                      }
                    case None => println("Error: could not parse actor path "+string)
                  }
                }}.orElse(Try{println("Error: remote holders data not parsed")})
                case _ => println("Error: remote holders data not parsed")
              }
            case DiffuseDataSpec.messageCode =>
              data match {
                case value:(Mac,Array[Byte])@unchecked => Try{
                  val mac = value._1
                  val msgBytes = value._2
                  Try{serializer.diffuseFromBytes(msgBytes)} match {
                    case Success(msg) =>
                      getRefs(msg._1,msg._2) match {
                        case Some((s:ActorRefWrapper,r:ActorRefWrapper)) =>
                          Try{ActorPath.fromString(msg._3)}.toOption match {
                            case Some(ref:ActorPath) => if (!r.remote && !bootStrapJobs.contains(r)) {
                              val msgHash = ByteArrayWrapper(fch.hash(Bytes.concat(
                                serializer.getBytes(mac.time),
                                msgBytes,
                                ecx.scalarMult(sk_ecx,pathToPeer(s.actorPath)._2)
                              )))
                              if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                                val peerInfo = pathToPeer(s.actorPath)
                                pathToPeer -= s.actorPath
                                pathToPeer += (s.actorPath->(peerInfo._1,peerInfo._2,mac.time))
                                updatePeerInfo()
                                context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                                  DiffuseData(msg._4,ActorRefWrapper(ref),msg._5,s)
                                )(context.system.dispatcher,self)
                              } else {
                                println("Error: Diffuse MAC failed")
                              }
                            }
                            case _ => println("Error: Diffuse message path not valid")
                          }
                        case None => println("Error: Diffuse message not parsed")
                      }
                    case _ => println("Error: Diffuse message parse failed")
                  }
                }.orElse(Try{println("Error: Diffuse message not valid")})
                case _ => println("Error: Diffuse message not parsed")
              }
            case HelloSpec.messageCode =>
              data match {
                case value:(Mac,Array[Byte])@unchecked => Try{
                  val mac = value._1
                  val msgBytes = value._2
                  Try{serializer.helloFromBytes(msgBytes)} match {
                    case Success(msg) =>
                      getRefs(msg._1,msg._2) match {
                        case Some((s:ActorRefWrapper,r:ActorRefWrapper)) => if (!r.remote && !bootStrapJobs.contains(r)) {
                          val msgHash = ByteArrayWrapper(fch.hash(Bytes.concat(
                            serializer.getBytes(mac.time),
                            msgBytes,
                            ecx.scalarMult(sk_ecx,pathToPeer(s.actorPath)._2)
                          )))
                          if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                            val peerInfo = pathToPeer(s.actorPath)
                            pathToPeer -= s.actorPath
                            pathToPeer += (s.actorPath->(peerInfo._1,peerInfo._2,mac.time))
                            updatePeerInfo()
                            context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                              Hello(msg._3,s)
                            )(context.system.dispatcher,self)
                          } else {
                            println("Error: Hello MAC failed")
                          }
                        }
                        case None => println("Error: Hello message not parsed")
                      }
                    case _ => println("Error: Hello message parse failed")
                  }
                }.orElse(Try{println("Error: Hello message not valid")})
                case _ => println("Error: Hello message not parsed")
              }
            case RequestBlockSpec.messageCode =>
              data match {
                case value:(Mac,Array[Byte])@unchecked => Try{
                  val mac = value._1
                  val msgBytes = value._2
                  Try{serializer.requestBlockFromBytes(msgBytes)} match {
                    case Success(msg) =>
                      getRefs(msg._1,msg._2) match {
                        case Some((s:ActorRefWrapper,r:ActorRefWrapper)) => if (!r.remote && !bootStrapJobs.contains(r)) {
                          val msgHash = ByteArrayWrapper(fch.hash(Bytes.concat(
                            serializer.getBytes(mac.time),
                            msgBytes,
                            ecx.scalarMult(sk_ecx,pathToPeer(s.actorPath)._2)
                          )))
                          if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                            val peerInfo = pathToPeer(s.actorPath)
                            pathToPeer -= s.actorPath
                            pathToPeer += (s.actorPath->(peerInfo._1,peerInfo._2,mac.time))
                            updatePeerInfo()
                            context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                              RequestBlock(msg._3,msg._4,s)
                            )(context.system.dispatcher,self)
                          } else {
                            println("Error: RequestBlock MAC failed")
                          }
                        }
                        case None => println("Error: RequestBlock message not parsed")
                      }
                    case _ => println("Error: RequestBlock message parse failed")
                  }


                }.orElse(Try{println("Error: RequestBlock message not valid")})
                case _ => println("Error: RequestBlock message not parsed")
              }
            case RequestTineSpec.messageCode =>
              data match {
                case value:(Mac,Array[Byte])@unchecked => Try{
                  val mac = value._1
                  val msgBytes = value._2
                  Try{serializer.requestTineFromBytes(msgBytes)} match {
                    case Success(msg) =>
                      getRefs(msg._1,msg._2) match {
                        case Some((s:ActorRefWrapper,r:ActorRefWrapper)) => if (!r.remote && !bootStrapJobs.contains(r)) {
                          val msgHash = ByteArrayWrapper(fch.hash(Bytes.concat(
                            serializer.getBytes(mac.time),
                            msgBytes,
                            ecx.scalarMult(sk_ecx,pathToPeer(s.actorPath)._2)
                          )))
                          if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                            val peerInfo = pathToPeer(s.actorPath)
                            pathToPeer -= s.actorPath
                            pathToPeer += (s.actorPath->(peerInfo._1,peerInfo._2,mac.time))
                            updatePeerInfo()
                            context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                              RequestTine(msg._3,msg._4,msg._5,s)
                            )(context.system.dispatcher,self)
                          } else {
                            println("Error: RequestTine MAC failed")
                          }
                        }
                        case None => println("Error: RequestTine message not parsed")
                      }
                    case _ => println("Error: RequestTine message parse failed")
                  }

                }.orElse(Try{println("Error: RequestTine message not valid")})
                case _ => println("Error: RequestTine message not parsed")
              }
            case ReturnBlocksSpec.messageCode =>
              data match {
                case value:(Mac,Array[Byte])@unchecked => Try{
                  val mac = value._1
                  val msgBytes = value._2
                  Try{serializer.returnBlocksFromBytes(msgBytes)} match {
                    case Success(msg) =>
                      getRefs(msg._1,msg._2) match {
                        case Some((s:ActorRefWrapper,r:ActorRefWrapper)) => if (!r.remote && !bootStrapJobs.contains(r)) {
                          val msgHash = ByteArrayWrapper(fch.hash(Bytes.concat(
                            serializer.getBytes(mac.time),
                            msgBytes,
                            ecx.scalarMult(sk_ecx,pathToPeer(s.actorPath)._2)
                          )))
                          if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                            val peerInfo = pathToPeer(s.actorPath)
                            pathToPeer -= s.actorPath
                            pathToPeer += (s.actorPath->(peerInfo._1,peerInfo._2,mac.time))
                            updatePeerInfo()
                            context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                              ReturnBlocks(msg._3,msg._4,s)
                            )(context.system.dispatcher,self)
                          } else {
                            println("Error: ReturnBlocks MAC failed")
                          }
                        }
                        case None => println("Error: ReturnBlocks message not parsed")
                      }
                    case _ => println("Error: ReturnBlocks message parse failed")
                  }
                }.orElse(Try{println("Error: ReturnBlocks message not valid")})
                case _ => println("Error: ReturnBlocks message not parsed")
              }
            case SendBlockSpec.messageCode =>
              data match {
                case value:(Mac,Array[Byte])@unchecked => Try{
                  val mac = value._1
                  val msgBytes = value._2
                  Try{serializer.sendBlockFromBytes(msgBytes)} match {
                    case Success(msg) =>
                      getRefs(msg._1,msg._2) match {
                        case Some((s:ActorRefWrapper,r:ActorRefWrapper)) => if (!r.remote && !bootStrapJobs.contains(r)) {
                          val msgHash = ByteArrayWrapper(fch.hash(Bytes.concat(
                            serializer.getBytes(mac.time),
                            msgBytes,
                            ecx.scalarMult(sk_ecx,pathToPeer(s.actorPath)._2)
                          )))
                          if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                            val peerInfo = pathToPeer(s.actorPath)
                            pathToPeer -= s.actorPath
                            pathToPeer += (s.actorPath->(peerInfo._1,peerInfo._2,mac.time))
                            updatePeerInfo()
                            context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                              SendBlock(msg._3,s)
                            )(context.system.dispatcher,self)
                          } else {
                            println("Error: SendBlock MAC failed")
                          }
                        }
                        case None => println("Error: SendBlock message not parsed")
                      }
                    case _ => println("Error: SendBlock message parse failed")
                  }

                }.orElse(Try{println("Error: SendBlock message not valid")})
                case _ => println("Error: SendBlock message not parsed")
              }
            case SendTxSpec.messageCode =>
              data match {
                case value:(Mac,Array[Byte])@unchecked => Try{
                  val mac = value._1
                  val msgBytes = value._2
                  Try{serializer.sendTxFromBytes(msgBytes)} match {
                    case Success(msg) =>
                      getRefs(msg._1,msg._2) match {
                        case Some((s:ActorRefWrapper,r:ActorRefWrapper)) => if (!r.remote && !bootStrapJobs.contains(r)) {
                          val msgHash = ByteArrayWrapper(fch.hash(Bytes.concat(
                            serializer.getBytes(mac.time),
                            msgBytes,
                            ecx.scalarMult(sk_ecx,pathToPeer(s.actorPath)._2)
                          )))
                          if (msgHash == mac.hash && mac.time > pathToPeer(s.actorPath)._3) {
                            val peerInfo = pathToPeer(s.actorPath)
                            pathToPeer -= s.actorPath
                            pathToPeer += (s.actorPath->(peerInfo._1,peerInfo._2,mac.time))
                            updatePeerInfo()
                            context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                              SendTx(msg._3,s)
                            )(context.system.dispatcher,self)
                          } else {
                            println("Error: SendTx MAC failed")
                          }
                        }
                        case None => println("Error: SendTx paths not valid")
                      }
                    case _ => println("Error: SendTx message parse failed")
                  }
                }.orElse(Try{println("Error: SendTx message not valid")})
                case _ => println("Error: SendTx message not parsed")
              }
            case _ => println("Error: message code did not match any specs")
          }
      }
  }

  def holdersToNetwork():Unit = {
    sendToNetwork[HoldersType,HoldersFromRemoteSpec.type](
      HoldersFromRemoteSpec,
      (
        holders.filterNot(_.remote).map(_.path.toString),
        pk_ecx,
        nextMsgTime()
      )
    )
  }

  private def holdersFromLocal: Receive = {
    /** accepts list of other holders from coordinator */
    case HoldersFromLocal(list:List[ActorRefWrapper]) =>
      val name = SharedData.scorexSettings.get.network.agentName
      for (holder<-list) {
        if (!holders.contains(holder)) {
          holders ::= holder
          SharedData.guiPeerInfo.get(name) match {
            case Some(list:List[ActorRefWrapper]) =>
              val newList = holder::list
              SharedData.guiPeerInfo -= name
              SharedData.guiPeerInfo += (name -> newList)
            case None => SharedData.guiPeerInfo += (name -> List(holder))
          }
        }
      }
      for (holder<-holders.filterNot(_.remote)) {
        if (!holdersPosition.keySet.contains(holder)) {
          holdersPosition += (holder->(rng.nextDouble()*180.0-90.0,rng.nextDouble()*360.0-180.0))
          println("Local holder given position")
        }
      }
      if (useFencing) {
        for (holder<-holders.filterNot(_.remote)) {
          if (!holderReady.keySet.contains(holder)) {
            holderReady += (holder->false)
          }
        }
      }
      if (!holders.forall(_.remote)) {
        timers.startPeriodicTimer(ActorPathSendTimerKey, ActorPathSendTimerKey, 10.seconds)
        holdersToNetwork()
      }
      updatePeerInfo()
      sender() ! "done"
  }


  private def messageFromLocal: Receive = {
    /** adds delay to locally routed message*/
    case MessageFromLocalToLocal(s,r,c) if !s.remote && !r.remote =>
      context.system.scheduler.scheduleOnce(delay(s,r,c),r.actorRef,c)(context.system.dispatcher,sender())

    case MessageFromLocalToRemote(from,r,command) if pathToPeer.keySet.contains(r) && !from.remote =>
      router match {
        case Some(rt) => rt.route(MessageFromLocalToRemote(from,r,command),sender)
        case None =>
          val s = from.actorPath
          command match {
            case c:DiffuseData =>
              val content:DiffuseDataType = (s.toString,r.toString,c.ref.actorPath.toString,c.sid,c.pks)
              val msgBytes = serializer.diffuseToBytes(content)
              val msgTime = nextMsgTime()
              val mac = Mac(ByteArrayWrapper(fch.hash(Bytes.concat(
                serializer.getBytes(msgTime),
                msgBytes,
                ecx.scalarMult(sk_ecx,pathToPeer(r)._2)
              ))),msgTime)
              sendToNetwork[(Mac,Array[Byte]),DiffuseDataSpec.type](DiffuseDataSpec,(mac,msgBytes),r)
            case c:Hello =>
              val content:HelloDataType = (s.toString,r.toString,c.slot)
              val msgBytes = serializer.helloToBytes(content)
              val msgTime = nextMsgTime()
              val mac = Mac(ByteArrayWrapper(fch.hash(Bytes.concat(
                serializer.getBytes(msgTime),
                msgBytes,
                ecx.scalarMult(sk_ecx,pathToPeer(r)._2)
              ))),msgTime)
              sendToNetwork[(Mac,Array[Byte]),HelloSpec.type](HelloSpec,(mac,msgBytes),r)
            case c:RequestBlock =>
              val content:RequestBlockType = (s.toString,r.toString,c.id,c.job)
              val msgBytes = serializer.requestBlockToBytes(content)
              val msgTime = nextMsgTime()
              val mac = Mac(ByteArrayWrapper(fch.hash(Bytes.concat(
                serializer.getBytes(msgTime),
                msgBytes,
                ecx.scalarMult(sk_ecx,pathToPeer(r)._2)
              ))),msgTime)
              sendToNetwork[(Mac,Array[Byte]),RequestBlockSpec.type](RequestBlockSpec,(mac,msgBytes),r)
            case c:RequestTine =>
              val content:RequestTineType = (s.toString,r.toString,c.id,c.depth,c.job)
              val msgBytes = serializer.requestTineToBytes(content)
              val msgTime = nextMsgTime()
              val mac = Mac(ByteArrayWrapper(fch.hash(Bytes.concat(
                serializer.getBytes(msgTime),
                msgBytes,
                ecx.scalarMult(sk_ecx,pathToPeer(r)._2)
              ))),msgTime)
              sendToNetwork[(Mac,Array[Byte]),RequestTineSpec.type](RequestTineSpec,(mac,msgBytes),r)
            case c:ReturnBlocks =>
              val content:ReturnBlocksType = (s.toString,r.toString,c.blocks,c.job)
              val msgBytes = serializer.returnBlocksToBytes(content)
              val msgTime = nextMsgTime()
              val mac = Mac(ByteArrayWrapper(fch.hash(Bytes.concat(
                serializer.getBytes(msgTime),
                msgBytes,
                ecx.scalarMult(sk_ecx,pathToPeer(r)._2)
              ))),msgTime)
              sendToNetwork[(Mac,Array[Byte]),ReturnBlocksSpec.type](ReturnBlocksSpec,(mac,msgBytes),r)
            case c:SendBlock =>
              val content:SendBlockType = (s.toString,r.toString,c.block)
              val msgBytes = serializer.sendBlockToBytes(content)
              val msgTime = nextMsgTime()
              val mac = Mac(ByteArrayWrapper(fch.hash(Bytes.concat(
                serializer.getBytes(msgTime),
                msgBytes,
                ecx.scalarMult(sk_ecx,pathToPeer(r)._2)
              ))),msgTime)
              sendToNetwork[(Mac,Array[Byte]),SendBlockSpec.type](SendBlockSpec,(mac,msgBytes),r)
            case c:SendTx =>
              val content:SendTxType = (s.toString,r.toString,c.transaction)
              val msgBytes = serializer.sendTxToBytes(content)
              val msgTime = nextMsgTime()
              val mac = Mac(ByteArrayWrapper(fch.hash(Bytes.concat(
                serializer.getBytes(msgTime),
                msgBytes,
                ecx.scalarMult(sk_ecx,pathToPeer(r)._2)
              ))),msgTime)
              sendToNetwork[(Mac,Array[Byte]),SendTxSpec.type](SendTxSpec,(mac,msgBytes),r)
            case _ =>
          }
      }
  }

  private def sendToNetwork[Content,Spec<:MessageSpec[Content]](spec:Spec,c:Content,r:ActorPath):Unit = {
    Try{spec.toBytes(c)}.toOption match {
      case Some(bytes:Array[Byte]) =>
        pathToPeer.get(r) match {
          case Some((peerName,_,_)) => networkController ! SendToNetwork(
            Message(spec,Left(bytes),None),SendToPeerByName(peerName,self)
          )
          case None =>
        }
      case None =>
    }
  }

  private def sendToNetwork[Content,Spec<:MessageSpec[Content]](spec:Spec,c:Content):Unit = {
    Try{spec.toBytes(c)}.toOption match {
      case Some(bytes:Array[Byte]) =>
        networkController ! SendToNetwork(Message(spec,Left(bytes),None),BroadcastExceptOfByName("bootstrap"))
      case None =>
    }
  }

  private def registerNC: Receive = {
    case InvalidateHolders(peerName) =>
      var holdersOut:List[ActorRefWrapper] = holders.filterNot(_.remote)
      for (holder <- holders) if (pathToPeer.keySet.contains(holder.actorPath)) {
        println(pathToPeer(holder.actorPath)._1,holder.actorPath)
        if (pathToPeer(holder.actorPath)._1 == peerName) {
          pathToPeer -= holder.actorPath
        } else {
          holdersOut ::= holder
        }
      }
      if (useGui) Try{SharedData.guiPeerInfo -= peerName}
      println("Peer removed: "+peerName)
      holders = holdersOut
      coordinatorRef ! HoldersFromRemote(holders)
      updatePeerInfo()
    case Register =>
      networkController ! RegisterMessageSpecs(prosomoMessageSpecs, self)
      router = Some({
        var i = 0
        val routees = Vector.fill(numMessageProcessors) {
          val ref = context.actorOf(RouteProcessor.props(fch.hash(seed+s"$i"),inputRef.map(_.actorRef)++Seq(self,coordinatorRef.actorRef)), s"Remote_${i}")
          i += 1
          ActorRefRoutee(ref)
        }
        Router(SmallestMailboxRoutingLogic(),routees)
      })
      sender() ! "done"
    case BootstrapJob(bootStrapper) =>
      if (bootStrapJobs.contains(bootStrapper)) {
        bootStrapJobs -= bootStrapper
      } else {
        bootStrapJobs += bootStrapper
      }
      updatePeerInfo()
  }

  def updatePeerInfo():Unit = {
    router match {
      case Some(rt) => rt.routees.foreach(r=>r.send(RouterPeerInfo(pathToPeer, bootStrapJobs, holders),self))
      case None => context.parent ! RouterPeerInfo(pathToPeer, bootStrapJobs, holders)
    }
  }

  def syncPeerInfo:Receive = {
    case value:RouterPeerInfo =>
      pathToPeer = value.pathToPeer
      bootStrapJobs = value.bootStrapJobs
      holders = value.holders
      router match {
        case Some(rt) => rt.routees.foreach(r=>r.send(RouterPeerInfo(pathToPeer, bootStrapJobs, holders),self))
        case None =>
      }
  }

  def receive: Receive =
    registerNC orElse
      syncPeerInfo orElse
      holdersFromLocal orElse
      messageFromLocal orElse
      messageFromPeer orElse
      routerReceive orElse {
      case _ =>
    }
}

object RouteProcessor {
  def props(seed:Array[Byte],ref:Seq[akka.actor.ActorRef]): Props =
    Props(new RouteProcessor(seed,ref.map(ActorRefWrapper.routerRef)))
}