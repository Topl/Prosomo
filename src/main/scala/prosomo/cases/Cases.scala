package prosomo.cases

import akka.actor.ActorRef
import prosomo.components.{Block, Transaction}
import prosomo.primitives.Types._
import prosomo.primitives.Mac

// case objects and classes for pattern matching messages between actors
case object Diffuse
case object Inbox
case object CloseDataFile
case object Status
case object Run
case object RequestKeys
case object GetTime
case object Update
case object WriteFile
case object StallActor
case object ReadCommand
case object Verify
case object RequestGossipers
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

//signed messages between holders, messages from remote
case class DiffuseData(ref:ActorRef,pks:PublicKeys,mac:Mac)
case class Hello(id:ActorRef,mac:Mac)
case class RequestBlock(id:SlotId,mac:Mac,job:Int)
case class RequestBlocks(id:SlotId,depth:Int,mac:Mac,job:Int)
case class ReturnBlocks(blocks:List[Block],mac:Mac,job:Int)
case class SendBlock(block:Block,mac:Mac)
case class SendTx(transaction:Transaction)

//messages between coordinator/router and holders
case class GetSlot(s:Int)
case class CoordRef(ref: ActorRef)
case class RouterRef(ref:ActorRef)
case class GetTime(t1:Long)
case class Initialize(tMax:Int)
case class SetClock(t0:Long)
case class GenBlock(b: Block)
case class IssueTx(s:Any)
case class WriteFile(fw: Any)
case class NewGraphFile(name:String)
case class GetGossipers(list:Any)
case class Party(list:Any,clear:Boolean)
case class GetState(s:Any)
case class GetBlockTree(t:Any,h:Any)
case class GetPositionData(s:Any)
case class Adversary(s:String)

