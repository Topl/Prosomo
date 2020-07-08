package prosomo.stakeholder

import akka.actor.{Actor, PoisonPill, Props, Timers}
import prosomo.cases.{MessageFromLocalToLocal, MessageFromLocalToLocalId, MessageFromLocalToRemote, ReturnBlocks, DiffuseData}
import prosomo.components.{Block, Serializer, Tine}
import prosomo.history.BlockStorage
import prosomo.primitives.{Fch, SharedData, Sig, SimpleTypes, Types}
import prosomo.primitives.Parameters.{printFlag, useFencing, useRouting, requestTineInterval}

import scala.math.BigInt
import scala.util.control.Breaks.{break, breakable}
import scala.util.Random

/**
  * AMS 2020:
  * Provider for remote TinePool syncing, used only for bootstrapping peers
  * @param blockStorage block database to be checked, should be thread safe with read/write locks
  * @param routerRef actor ref to send network messages to
  */

class TineProvider(blockStorage: BlockStorage,localRef:ActorRefWrapper)(implicit routerRef:ActorRefWrapper)
  extends Actor with Timers with Types {
  import TineProvider.Info
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
        localRef ! MessageFromLocalToLocal(sender, ref, command)
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
    tine:Option[Tine],
    inbox:Option[Map[Sid,(ActorRefWrapper,PublicKeys)]]
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
              blockStorage.restoreBlock(id) match {
                case Some(block:Block) =>
                  returnedIdList ::= id
                  send(
                    holderRef,
                    ref,
                    ReturnBlocks(List(block),-1,holderRef)
                  )
                case None => break
              }
              if (!useFencing) Thread.sleep(requestTineInterval)
            } else {
              break
            }
          }
        }
        inbox match {
          case Some(data) =>
            for (entry <- data) {
              send(
                holderRef,
                ref,
                DiffuseData(entry._1,entry._2._1,entry._2._2,holderRef)
              )
              Thread.sleep(requestTineInterval)
            }
          case None =>
        }
      } else {
        breakable{
          while (returnedIdList.length < depth) {
            blockStorage.restoreBlock(id) match {
              case Some(block:Block) =>
                returnedIdList ::= id
                send(
                  holderRef,
                  ref,
                  ReturnBlocks(List(block), job,holderRef)
                )
                id = block.parentSlotId
              case None => break
            }
            if (!useFencing) Thread.sleep(requestTineInterval)
          }
        }
      }
      if (holderIndex == SharedData.printingHolder && printFlag) {
        println("Holder " + holderIndex.toString + " Returned Tine")
      }
      self ! PoisonPill
  }

  override def postStop(): Unit = {
    context.parent ! TineProvider.Done
  }
}

object TineProvider extends SimpleTypes {
  case class Info(
    holderIndex:Int,
    ref:ActorRefWrapper,
    holderRef:ActorRefWrapper,
    startId:SlotId,
    depth:Int,
    job:Int,
    tine:Option[Tine],
    inbox:Option[Map[Sid,(ActorRefWrapper,PublicKeys)]]
  )
  case object Done
  def props(blockStorage: BlockStorage,localRef:ActorRefWrapper)(implicit routerRef:ActorRefWrapper):Props =
    Props(new TineProvider(blockStorage,localRef))
}