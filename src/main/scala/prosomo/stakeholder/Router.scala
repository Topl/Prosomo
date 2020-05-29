package prosomo.stakeholder

import akka.actor.{Actor, ActorPath, Props, Timers}
import akka.util.Timeout
import prosomo.cases._
import prosomo.components.Serializer
import prosomo.primitives.{Distance, Parameters, SharedData, Types}
import prosomo.remote.SpecTypes.{DiffuseDataType, HelloDataType, RequestBlockType, RequestTineType, ReturnBlocksType, SendBlockType, SendTxType}
import prosomo.remote.{DiffuseDataSpec, _}
import scorex.util.encode.Base58
import scala.collection.immutable.ListMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.math.BigInt
import scala.util.{Failure, Random, Success, Try}
import scorex.core.network._
import scorex.core.network.ConnectedPeer
import scorex.core.network.message.{MessageSpec,Message}
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.NetworkController.ReceivableMessages.{SendToNetwork,RegisterMessageSpecs}


class Router(seed:Array[Byte],inputRef:Seq[ActorRefWrapper]) extends Actor
  with Types
  with Timers {
  import Parameters._
  val serializer:Serializer = new Serializer
  var holders:List[ActorRefWrapper] = List()
  val rng = new Random(BigInt(seed).toLong)
  var holdersPosition:Map[ActorRefWrapper,(Double,Double)] = Map()
  var distanceMap:Map[(ActorRefWrapper,ActorRefWrapper),Long] = Map()
  var holderMessages:Map[Slot,Map[Long,Map[ActorRefWrapper,Map[BigInt,(ActorRefWrapper,ActorRefWrapper,Any)]]]] = Map()
  var holderReady:Map[ActorRefWrapper,Boolean] = Map()
  var globalSlot:Slot = 0
  var localSlot:Slot = 0
  var coordinatorRef:ActorRefWrapper = _
  val networkController:ActorRefWrapper = inputRef(0)
  val peerManager:ActorRefWrapper = inputRef(1)
  var t0:Long = 0
  var ts:Long = 0
  var roundDone = true
  var firstDataPass = true
  var roundStep = "updateSlot"
  val printSteps = false
  var txRoundCounter = 0
  var maxDelay:Double = 0
  var transactionCounter:Int = 0
  var pathToPeer:Map[ActorPath,String] = Map()
  var connectedPeer:Set[ConnectedPeer] = Set()
  implicit val routerRef = ActorRefWrapper.routerRef(self)

  private case object ActorPathSendTimerKey

  /**
    * Sends commands one by one to list of stakeholders
    * @param holders actor list
    * @param command object to be sent
    */
  def sendAssertDone(holders:List[ActorRefWrapper], command: Any) = {
    for (holder <- holders){
      implicit val timeout:Timeout = Timeout(waitTime)
      val future = holder ? command
      val result = Await.result(future, timeout.duration)
      assert(result == "done")
    }
  }

  /**
    * Sends command to stakeholder and waits for response
    * @param holder
    * @param command
    */
  def sendAssertDone(holder:ActorRefWrapper, command: Any) = {
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

  def reset:Unit = {
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
    val delay_ns:Long = (rng.nextDouble()*delay_ms_noise*1.0e6).toLong + (serializer.getAnyBytes(data).length*delay_ms_byte*1.0e6).toLong + distanceMap((sender,recip))
    if (delay_ns/1.0e9 > maxDelay) {maxDelay = delay_ns/1.0e9}
    delay_ns.nano
  }

  def deliver = {
    var slotMessages = holderMessages(globalSlot)
    val next_message_t = slotMessages.keySet.min
    if (next_message_t > (txRoundCounter*commandUpdateTime.toNanos)) {
      txRoundCounter += 1
      ts = txRoundCounter*commandUpdateTime.toNanos
      issueTx
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
          if (!r.remote) context.system.scheduler.scheduleOnce(0.nano,r.actorRef,c)(context.system.dispatcher,s.actorRef)
          if (messageMap.nonEmpty) queue += (holder->messageMap)
        }
      }
      if (queue.nonEmpty) slotMessages += (ts->queue)
      if (slotMessages.nonEmpty) holderMessages += (globalSlot -> slotMessages)
    }
  }

  /**randomly picks two holders and creates a transaction between the two*/
  def issueTx = {
    for (i <- 0 to txProbability.floor.toInt) {
      val holder1 = rng.shuffle(holders.filterNot(_.remote)).head
      val r = rng.nextDouble
      if (r<txProbability%1.0) {
        val holder2 = holders.filter(_ != holder1)(rng.nextInt(holders.length-1))
        assert(holder1 != holder2)
        val delta:BigInt = BigDecimal(maxTransfer*rng.nextDouble).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        reset(holder1)
        transactionCounter += 1
        context.system.scheduler.scheduleOnce(0.nano,holder1.actorRef,IssueTx(holder2,delta))(context.system.dispatcher,self)
      }
    }
  }

  def update = {
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
        reset
        sendAssertDone(holders.filterNot(_.remote),GetSlot(globalSlot))
      } else {
        roundStep match {
          case "updateSlot" => {
            if (holdersReady) {
              roundStep = "passData"
              reset
            }
          }
          case "passData" => {
            if (holdersReady) {
              if (holderMessages.keySet.contains(globalSlot)) {
                if (printSteps) println("-------deliver---------")
                deliver
              } else {
                if (slotT*1000000>(txRoundCounter*commandUpdateTime.toNanos)) {
                  txRoundCounter += 1
                  ts = txRoundCounter*commandUpdateTime.toNanos
                  issueTx
                } else {
                  roundStep = "endStep"
                  if (printSteps) println("---------end-----------")
                  reset
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

    case Flag(ref,value) => {
//      if (value == "updateChain" || value == "passData") {if (printSteps) println(value+" "+holders.indexOf(sender).toString)
//        for (holder<-holders) {
//          if (printSteps) println(holders.indexOf(holder).toString+" "+holderReady(holder))
//        }
//        if (printSteps) println(holderMessages.keySet.contains(globalSlot))
//      }
      if (value == roundStep && holderReady.keySet.contains(ref)) {
        holderReady -= ref
        holderReady += (ref -> true)
      }
    }

    case NextSlot => {
      if (roundDone) globalSlot += 1
      roundDone = false
    }

    case MessageFromLocalToLocalId(uid,s,r,c) => {
      val newMessage = (s,r,c)
      val nsDelay = delay(s,r,c)
      val messageDelta:Slot = ((nsDelay.toNanos+ts)/(slotT*1000000)).toInt
      val priority:Long = (nsDelay.toNanos+ts)%(slotT*1000000)
      val offsetSlot = globalSlot+messageDelta
      val messages:Map[Long,Map[ActorRefWrapper,Map[BigInt,(ActorRefWrapper,ActorRefWrapper,Any)]]] = if (holderMessages.keySet.contains(offsetSlot)) {
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
    }

    case Run => {
      timers.startPeriodicTimer(ActorPathSendTimerKey, Update, 1.nano)
      coordinatorRef ! NextSlot
    }

    case value:CoordRef => {
      coordinatorRef = value.ref
      sender() ! "done"
    }

    case value:String => if (value == "fence_step") {
      println(roundStep)
      sender() ! "done"
    }

    case Update => update

    case ActorPathSendTimerKey => {
      if (holders.filterNot(_.remote).nonEmpty) toNetwork[List[String],HoldersFromRemoteSpec.type](HoldersFromRemoteSpec,holders.filterNot(_.remote).map(_.path.toString))
      holders.filterNot(_.remote).foreach(_ ! NewGossipers)
    }

    case value:SetClock => {
      t0 = value.t0
      sender() ! "done"
    }

    case value:GetTime => {
      globalSlot = ((value.t1 - t0) / slotT).toInt
    }

    case RequestPositionData => {
      sender() ! GetPositionData((holdersPosition,distanceMap))
    }

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
    case DataFromPeer(spec, data, remote) => {
      spec.messageCode match {
        case DiffuseDataSpec.messageCode =>{
          data match {
            case msg:DiffuseDataType@unchecked =>
              getRefs(msg._1,msg._2) match {
                case Some((s:ActorRefWrapper,r:ActorRefWrapper)) =>
                  if (!r.remote) context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                    DiffuseData(s,msg._3,msg._4)
                  )(context.system.dispatcher,self)
                case None =>
              }
            case _ =>
          }
        }
        case HelloSpec.messageCode => {
          data match {
            case msg:HelloDataType@unchecked =>
              getRefs(msg._1,msg._2) match {
                case Some((s:ActorRefWrapper,r:ActorRefWrapper)) =>
                  if (!r.remote) context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                    Hello(s,msg._3)
                  )(context.system.dispatcher,self)
                case None => println("error: Hello message not parsed")
              }
            case _ => println("error: Hello message not parsed")
          }
        }
        case RequestBlockSpec.messageCode => {
          data match {
            case msg:RequestBlockType@unchecked =>
              getRefs(msg._1,msg._2) match {
                case Some((s:ActorRefWrapper,r:ActorRefWrapper)) =>
                  if (!r.remote) context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                    RequestBlock(msg._3,msg._4,msg._5)
                  )(context.system.dispatcher,self)
                case None => println("error: RequestBlock message not parsed")
              }
            case _ => println("error: RequestBlock message not parsed")
          }
        }
        case RequestTineSpec.messageCode => {
          data match {
            case msg:RequestTineType@unchecked =>
              getRefs(msg._1,msg._2) match {
                case Some((s:ActorRefWrapper,r:ActorRefWrapper)) =>
                  if (!r.remote) context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                    RequestTine(msg._3,msg._4,msg._5,msg._6)
                  )(context.system.dispatcher,self)
                case None => println("error: RequestTine message not parsed")
              }
            case _ => println("error: RequestTine message not parsed")
          }
        }
        case ReturnBlocksSpec.messageCode => {
          data match {
            case msg:ReturnBlocksType@unchecked =>
              getRefs(msg._1,msg._2) match {
                case Some((s:ActorRefWrapper,r:ActorRefWrapper)) =>
                  if (!r.remote) context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                    ReturnBlocks(msg._3,msg._4,msg._5)
                  )(context.system.dispatcher,self)
                case None => println("error: ReturnBlocks message not parsed")
              }
            case _ => println("error: ReturnBlocks message not parsed")
          }
        }
        case SendBlockSpec.messageCode => {
          data match {
            case msg:SendBlockType@unchecked =>
              getRefs(msg._1,msg._2) match {
                case Some((s:ActorRefWrapper,r:ActorRefWrapper)) =>
                  if (!r.remote) context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                    SendBlock(msg._3,msg._4)
                  )(context.system.dispatcher,self)
                case None => println("error: SendBlock message not parsed")
              }
            case _ => println("error: SendBlock message not parsed")
          }
        }
        case SendTxSpec.messageCode => {
          data match {
            case msg:SendTxType@unchecked =>
              getRefs(msg._1,msg._2) match {
                case Some((s:ActorRefWrapper,r:ActorRefWrapper)) =>
                  if (!r.remote) context.system.scheduler.scheduleOnce(0.nanos,r.actorRef,
                    SendTx(msg._3)
                  )(context.system.dispatcher,self)
                case None => println("error: SendTx message not parsed")
              }
            case _ => println("error: SendTx message not parsed")
          }
        }
        case HoldersFromRemoteSpec.messageCode => {
          data match {
            case msg:List[String]@unchecked => {
              var toDiffuse = false
              for (string<-msg) {
                Try{ActorPath.fromString(string)}.toOption match {
                  case Some(newPath:ActorPath) => {
                    holders.find(_.path == newPath) match {
                      case None => {
                        holders ::= ActorRefWrapper(newPath)
                        pathToPeer += (newPath -> remote.peerInfo.get.peerSpec.agentName)
                        SharedData.guiPeerInfo.get(remote.peerInfo.get.peerSpec.agentName) match {
                          case Some(list:List[ActorRefWrapper]) => {
                            val newList = ActorRefWrapper(newPath)::list
                            SharedData.guiPeerInfo -= remote.peerInfo.get.peerSpec.agentName
                            SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> newList)
                          }
                          case None => SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> List(ActorRefWrapper(newPath)))
                        }
                        println("New holder "+newPath.toString)
                        coordinatorRef ! HoldersFromRemote(holders)
                        toDiffuse = true
                        if (holders.filterNot(_.remote).nonEmpty) toNetwork[List[String],HoldersFromRemoteSpec.type](HoldersFromRemoteSpec,holders.filterNot(_.remote).map(_.path.toString))
                      }
                      case Some(actorRef:ActorRefWrapper) => {
                        if (pathToPeer(actorRef.path) != remote.peerInfo.get.peerSpec.agentName) {
                          if (SharedData.guiPeerInfo.keySet.contains(pathToPeer(actorRef.path))) SharedData.guiPeerInfo -= pathToPeer(actorRef.path)
                          val key = actorRef.path
                          pathToPeer -= key
                          pathToPeer += (key -> remote.peerInfo.get.peerSpec.agentName)
                          toDiffuse = true
                          SharedData.guiPeerInfo.get(remote.peerInfo.get.peerSpec.agentName) match {
                            case Some(list:List[ActorRefWrapper]) => {
                              val newList = actorRef::list
                              SharedData.guiPeerInfo -= remote.peerInfo.get.peerSpec.agentName
                              SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> newList)
                            }
                            case None => SharedData.guiPeerInfo += (remote.peerInfo.get.peerSpec.agentName -> List(actorRef))
                          }
                          if (holders.filterNot(_.remote).nonEmpty) toNetwork[List[String],HoldersFromRemoteSpec.type](HoldersFromRemoteSpec,holders.filterNot(_.remote).map(_.path.toString))
                          println("Updated Peer "+newPath.toString)
                        }
                      }
                    }
                  }
                  case None => println("error: could not parse actor path")
                }
              }
              if (toDiffuse) holders.filterNot(_.remote).foreach(_ ! Diffuse)
            }
            case _ => println("error: remote holders data not parsed")
          }
        }
        case _ => println("error: message code did not match any specs")
      }
    }
  }

  private def holdersFromLocal: Receive = {
    /** accepts list of other holders from coordinator */
    case HoldersFromLocal(list:List[ActorRefWrapper]) => {
      val name = s"local_"+Parameters.prosomoNodeUID+" "+Parameters.declaredAddressFromRemote
      for (holder<-list) {
        if (!holders.contains(holder)) {
          holders ::= holder
          SharedData.guiPeerInfo.get(name) match {
            case Some(list:List[ActorRefWrapper]) => {
              val newList = holder::list
              SharedData.guiPeerInfo -= name
              SharedData.guiPeerInfo += (name -> newList)
            }
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
      if (holders.filterNot(_.remote).nonEmpty) {
        timers.startPeriodicTimer(ActorPathSendTimerKey, ActorPathSendTimerKey, 10.seconds)
        toNetwork[List[String],HoldersFromRemoteSpec.type](HoldersFromRemoteSpec,holders.filterNot(_.remote).map(_.path.toString))
      }
      sender() ! "done"
    }
  }


  private def messageFromLocal: Receive = {
    /** adds delay to locally routed message*/
    case MessageFromLocalToLocal(s,r,c) => {
      assert(!s.remote && !r.remote)
      context.system.scheduler.scheduleOnce(delay(s,r,c),r.actorRef,c)(context.system.dispatcher,sender())
    }

    case MessageFromLocalToRemote(sender,r,command) => {
      val s = sender.actorPath
      command match {
        case c:DiffuseData => {
          val content:DiffuseDataType = (c.ref.toString,r.toString,c.pks,c.mac)
          toNetwork[DiffuseDataType,DiffuseDataSpec.type](DiffuseDataSpec,content,r)
        }
        case c:Hello => {
          val content:HelloDataType = (c.ref.toString,r.toString,c.mac)
          toNetwork[HelloDataType,HelloSpec.type](HelloSpec,content,r)
        }
        case c:RequestBlock => {
          val content:RequestBlockType = (s.toString,r.toString,c.id,c.mac,c.job)
          toNetwork[RequestBlockType,RequestBlockSpec.type](RequestBlockSpec,content,r)
        }
        case c:RequestTine => {
          val content:RequestTineType = (s.toString,r.toString,c.id,c.depth,c.mac,c.job)
          toNetwork[RequestTineType,RequestTineSpec.type](RequestTineSpec,content,r)
        }
        case c:ReturnBlocks => {
          val content:ReturnBlocksType = (s.toString,r.toString,c.blocks,c.mac,c.job)
          toNetwork[ReturnBlocksType,ReturnBlocksSpec.type](ReturnBlocksSpec,content,r)
        }
        case c:SendBlock => {
          val content:SendBlockType = (s.toString,r.toString,c.block,c.mac)
          toNetwork[SendBlockType,SendBlockSpec.type](SendBlockSpec,content,r)
        }
        case c:SendTx => {
          val content:SendTxType = (s.toString,r.toString,c.transaction)
          toNetwork[SendTxType,SendTxSpec.type](SendTxSpec,content,r)
        }
        case _ =>
      }
    }
  }

  private def toNetwork[Content,Spec<:MessageSpec[Content]](spec:Spec,c:Content,r:ActorPath):Unit = {
    Try{spec.toBytes(c)}.toOption match {
      case Some(bytes:Array[Byte]) =>
        networkController ! SendToNetwork(Message(spec,Left(bytes),None),SendToPeerByName(pathToPeer(r)))
      case None =>
    }
  }

  private def toNetwork[Content,Spec<:MessageSpec[Content]](spec:Spec,c:Content):Unit = {
    Try{spec.toBytes(c)}.toOption match {
      case Some(bytes:Array[Byte]) =>
        networkController ! SendToNetwork(Message(spec,Left(bytes),None),BroadcastExceptOfByName("bootstrap"))
      case None =>
    }
  }

  private def registerNC: Receive = {
    case Register => {
      networkController ! RegisterMessageSpecs(messageSpecs, self)
      sender() ! "done"
    }
  }

  def receive: Receive =
    registerNC orElse
      holdersFromLocal orElse
      messageFromLocal orElse
      messageFromPeer orElse
      routerReceive orElse {
      case a: Any =>
    }

}

object Router {
  def props(seed:Array[Byte],ref:Seq[akka.actor.ActorRef]): Props = Props(new Router(seed,ref.map(ActorRefWrapper.routerRef(_))))
}