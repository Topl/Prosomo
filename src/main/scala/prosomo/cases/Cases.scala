package prosomo.cases

import akka.actor.ActorPath
import prosomo.stakeholder.ActorRefWrapper
import prosomo.components.{Block, Transaction}
import prosomo.primitives.Types._
import prosomo.primitives.Mac

import scala.math.BigInt

// case objects and classes for pattern matching messages between actors
case object Diffuse
case object Inbox
case object CloseDataFile
case object Status
case object Run
case object GetTime
case object Update
case object WriteFile
case object StallActor
case object ReadCommand
case object Verify
case object RequestGossipers
case object NewGossipers
case object RequestState
case object RequestBlockTree
case object Populate
case object NewDataFile
case object NextSlot
case object EndStep
case object RequestPositionData
case object GetBalance
case object Refresh
case object Register
case object Initialize
case object BootstrapJob

//signed messages between holders, messages from remote
case class DiffuseData(ref:ActorRefWrapper, pks:PublicKeys, mac:Mac)
case class Hello(ref:ActorRefWrapper, mac:Mac)
case class RequestBlock(id:SlotId,mac:Mac,job:Int)
case class RequestTine(id:SlotId, depth:Int, mac:Mac, job:Int)
case class ReturnBlocks(blocks:List[Block],mac:Mac,job:Int)
case class SendBlock(block:Block,mac:Mac)
case class SendTx(transaction:Transaction)

//messages between coordinator/router and holders
case class MessageFromLocalToRemote(r:ActorPath,c:Any)
case class MessageFromLocalToLocal(s:ActorRefWrapper,r:ActorRefWrapper,c:Any)
case class MessageFromLocalToLocalId(uid:BigInt,s:ActorRefWrapper,r:ActorRefWrapper,c:Any)
case class HoldersFromLocal(list:List[ActorRefWrapper])
case class HoldersFromRemote(list:List[ActorRefWrapper])
case class Flag(s:ActorRefWrapper,f:String)
case class GetSlot(s:Int)
case class CoordRef(ref: ActorRefWrapper)
case class GetTime(t1:Long)
case class SetClock(t0:Long)
case class GenBlock(b: Block)
case class IssueTx(ref:ActorRefWrapper,delta:BigInt)
case class WriteFile(fw: Any)
case class NewGraphFile(name:String)
case class GetGossipers(list:List[ActorRefWrapper])
case class Party(list:List[ActorRefWrapper],clear:Boolean)
case class GetState(s:State)
case class GetBlockTree(t:Any,h:Any)
case class GetPositionData(s:(Map[ActorRefWrapper,(Double,Double)],Map[(ActorRefWrapper,ActorRefWrapper),Long]))
case class Adversary(s:String)

