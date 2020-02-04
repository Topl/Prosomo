package prosomo

import akka.actor.{Actor, ActorRef, Props, Timers}
import akka.util.Timeout
import akka.pattern.ask
import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.immutable.ListMap
import scala.concurrent.Await
import scala.math.BigInt
import scala.util.Random
import scala.concurrent.duration._
import scorex.crypto.encode.Base58

class Router(seed:Array[Byte]) extends Actor
  with Parameters
  with Types
  with Timers {
  var holders:List[ActorRef] = List()
  val rng = new Random(BigInt(seed).toLong)
  var holdersPosition:Map[ActorRef,(Double,Double)] = Map()
  var distanceMap:Map[(ActorRef,ActorRef),Long] = Map()
  var holderMessages:Map[Slot,Map[Long,Map[ActorRef,Map[BigInt,(ActorRef,ActorRef,Any)]]]] = Map()
  var holderReady:Map[ActorRef,Boolean] = Map()
  var globalSlot:Slot = 0
  var localSlot:Slot = 0
  var coordinatorRef:ActorRef = _
  var t0:Long = 0
  var ts:Long = 0
  var roundDone = true
  var firstDataPass = true
  var roundStep = "updateSlot"
  val printSteps = false
  var txRoundCounter = 0
  var maxDelay:Double = 0
  var transactionCounter:Int = 0
  var holderKeys:Map[ActorRef,PublicKeyW] = Map()

  private case object timerKey

  /**
    * Sends commands one by one to list of stakeholders
    * @param holders actor list
    * @param command object to be sent
    */
  def sendAssertDone(holders:List[ActorRef], command: Any) = {
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
  def sendAssertDone(holder:ActorRef, command: Any) = {
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = holder ? command
    val result = Await.result(future, timeout.duration)
    assert(result == "done")
  }

  def holdersReady:Boolean = {
    var bool = true
    for (holder <- holders){
      bool &&= holderReady(holder)
    }
    bool
  }

  def reset:Unit = {
    for (holder <- holders){
      if (holderReady.keySet.contains(holder)) holderReady -= holder
      holderReady += (holder->false)
    }
  }

  def reset(holder:ActorRef):Unit = {
    if (holderReady.keySet.contains(holder)) holderReady -= holder
    holderReady += (holder->false)
  }

  def delay(sender:ActorRef,recip:ActorRef,data:Any):FiniteDuration = {
    if (!distanceMap.keySet.contains((sender,recip))) {
      distanceMap += ((sender,recip)->(delay_ms_km*1.0e6*DistanceCalculator.distance(
        holdersPosition(sender)._1,
        holdersPosition(sender)._2,
        holdersPosition(recip)._1,
        holdersPosition(recip)._2,
        "K")).toLong)
    }
    val delay_ns:Long = (rng.nextDouble()*delay_ms_noise*1.0e6).toLong + (serialize(data).length*delay_ms_byte*1.0e6).toLong + distanceMap((sender,recip))
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
      var queue:Map[ActorRef,Map[BigInt,(ActorRef,ActorRef,Any)]] = slotMessages(ts)
      slotMessages -= ts
      for (holder <- rng.shuffle(holders)) {
        if (queue.keySet.contains(holder)) {
          var messageMap:Map[BigInt,(ActorRef,ActorRef,Any)] = queue(holder)
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
              case value:SendBlock => Base58.encode(value.s match {case s:Box => {s._1 match {case bInfo: (Block,BlockId) => {bInfo._2._2.data}}}})
              case value:SendTx => Base58.encode(value.s match {case trans:Transaction => {trans._4.data}})
              case _ => " "
            }
          )
          context.system.scheduler.scheduleOnce(0 nano,r,c)(context.system.dispatcher,s)
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
      val holder1 = rng.shuffle(holders).head
      val r = rng.nextDouble
      if (r<txProbability%1.0) {
        val holder2 = holders.filter(_ != holder1)(rng.nextInt(holders.length-1))
        assert(holder1 != holder2)
        val delta:BigInt = BigDecimal(maxTransfer*rng.nextDouble).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        reset(holder1)
        transactionCounter += 1
        context.system.scheduler.scheduleOnce(0 nano,holder1,IssueTx((holderKeys(holder2),delta)))(context.system.dispatcher,self)
      }
    }
  }

  def update = {
    if (globalSlot > L_s || sharedData.killFlag) {
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
        sendAssertDone(holders,GetSlot(globalSlot))
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
                  for (holder<-holders) {
                    holder ! "endStep"
                  }
                }
              }
            } else {
              if (firstDataPass) {
                if (printSteps) println("---------first-----------")
                for (holder<-holders) {
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

  def receive: Receive = {

    case value:Map[ActorRef,PublicKeyW] => {
      holderKeys = value
      sender() ! "done"
    }

    case flag:(ActorRef,String) => {
      val (ref,value) = flag
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


    /** accepts list of other holders from coordinator */
    case list:List[ActorRef] => {
      holders = list
      for (holder<-holders) {
        if (!holdersPosition.keySet.contains(holder)) {
          holdersPosition += (holder->(rng.nextDouble()*180.0-90.0,rng.nextDouble()*360.0-180.0))
        }
      }
      if (useFencing) {
        for (holder<-holders) {
          if (!holderReady.keySet.contains(holder)) {
            holderReady += (holder->false)
          }
        }
      }
      sender() ! "done"
    }

    case NextSlot => {
      if (roundDone) globalSlot += 1
      roundDone = false
    }

    /** adds delay to routed message*/
    case newMessage:(ActorRef,ActorRef,Any) => {
      val (s,r,c) = newMessage
      context.system.scheduler.scheduleOnce(delay(s,r,c),r,c)(context.system.dispatcher,sender())
    }

    case newIdMessage:(BigInt,ActorRef,ActorRef,Any) => {
      val (uid,s,r,c) = newIdMessage
      val newMessage = (s,r,c)
      val nsDelay = delay(s,r,c)
      val messageDelta:Slot = ((nsDelay.toNanos+ts)/(slotT*1000000)).toInt
      val priority:Long = (nsDelay.toNanos+ts)%(slotT*1000000)
      val offsetSlot = globalSlot+messageDelta
      val messages:Map[Long,Map[ActorRef,Map[BigInt,(ActorRef,ActorRef,Any)]]] = if (holderMessages.keySet.contains(offsetSlot)) {
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
      timers.startPeriodicTimer(timerKey, Update, 1 nano)
      coordinatorRef ! NextSlot
    }

    case value:CoordRef => {
      value.ref match {
        case r: ActorRef => coordinatorRef = r
        case _ =>
      }
      sender() ! "done"
    }

    case value:String => if (value == "fence_step") {
      println(roundStep)
      sender() ! "done"
    }

    case Update => update

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

    case _ =>
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
}

object Router {
  def props(seed:Array[Byte]): Props = Props(new Router(seed))
}