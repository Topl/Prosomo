package prosomo.stakeholder

import akka.actor.{Actor, PoisonPill, Props, Timers}
import prosomo.cases.{MessageFromLocalToLocal, MessageFromLocalToLocalId, MessageFromLocalToRemote, ReturnBlocks}
import prosomo.components.{Block, Serializer, Tine}
import prosomo.history.BlockStorage
import prosomo.primitives.{Fch, SharedData, Sig, SimpleTypes, Types}
import prosomo.primitives.Parameters.{printFlag, useFencing, useRouting}

import scala.math.BigInt
import scala.util.control.Breaks.{break, breakable}
import scala.util.Random

/**
  * AMS 2020:
  * Provider for remote TinePool syncing, used only for bootstrapping peers
  * @param blockStorage block database to be checked, should be thread safe with read/write locks
  * @param routerRef actor ref to send network messages to
  */

class RequestTineProvider(blockStorage: BlockStorage)(implicit routerRef:ActorRefWrapper) extends Actor with Timers with Types {
  import RequestTineProvider.Info
  val sig:Sig = new Sig
  val rng:Random = new Random
  val serializer:Serializer = new Serializer
  override val fch = new Fch
  rng.setSeed(0L)

  def send(sender:ActorRefWrapper, ref:ActorRefWrapper, command: Any): Unit = {
    if (useRouting && !useFencing) {
      if (ref.remote) {
        routerRef ! MessageFromLocalToRemote(sender,ref.path, command)
      } else {
        routerRef ! MessageFromLocalToLocal(sender, ref, command)
      }
    } else if (useFencing) {
      routerRef ! MessageFromLocalToLocalId(BigInt(fch.hash(rng.nextString(64))),sender,ref,command)
    } else {
      ref ! command
    }
  }

  override def receive: Receive = {
    case Info(
    holderIndex:Int,
    ref:ActorRefWrapper,
    holderRef:ActorRefWrapper,
    startId:SlotId,
    depth:Int,
    job:Int,
    tine:Option[Tine]
    ) =>
      if (holderIndex == SharedData.printingHolder && printFlag) {
        println("Holder " + holderIndex.toString + " Was Requested Tine")
      }
      var returnedIdList:List[SlotId] = List()
      var id:SlotId = startId
      if (job == -1) {
        // job -1 means fetch info from hello message
        breakable{
          for (id <- tine.get.ordered) {
            if (returnedIdList.length < depth) {
              blockStorage.restore(id) match {
                case Some(block:Block) =>
                  returnedIdList ::= id
                  send(holderRef,ref,ReturnBlocks(List(block),-1,holderRef))
                case None => break
              }
              if (!useFencing) Thread.sleep(100)
            } else {
              break
            }
          }
          // job -2 means end of fetch info
          if (returnedIdList.length < depth) send(holderRef,ref,ReturnBlocks(List(),-2,holderRef))
        }
      } else {
        breakable{
          while (returnedIdList.length < depth) {
            blockStorage.restore(id) match {
              case Some(block:Block) =>
                returnedIdList ::= id
                send(holderRef,ref,ReturnBlocks(List(block),job,holderRef))
                id = block.parentSlotId
              case None => break
            }
            if (!useFencing) Thread.sleep(100)
          }
        }
      }
      if (holderIndex == SharedData.printingHolder && printFlag) {
        println("Holder " + holderIndex.toString + " Returned Tine")
      }
      self ! PoisonPill
  }

  override def postStop(): Unit = {
    context.parent ! RequestTineProvider.Done
  }
}

object RequestTineProvider extends SimpleTypes {
  case class Info(
    holderIndex:Int,
    ref:ActorRefWrapper,
    holderRef:ActorRefWrapper,
    startId:SlotId,
    depth:Int,
    job:Int,
    tine:Option[Tine]
  )
  case object Done
  def props(blockStorage: BlockStorage)(implicit routerRef:ActorRefWrapper):Props = Props(new RequestTineProvider(blockStorage))
}