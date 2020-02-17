package prosomo.cases

import akka.actor.ActorRef
import prosomo.components.{Block, Box, Transaction}
import prosomo.components.Types._

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

//signed messages between holders, messages from remote
case class Hello(id:ActorRef, box:Box)
case class SendBlock(block:Block,box:Box)
case class RequestBlock(id:BlockId,box:Box,job:Int)
case class RequestChain(id:BlockId,depth:Int,box:Box,job:Int)
case class ReturnBlock(blocks:List[Block],box:Box,job:Int)
case class DiffuseData(ref:ActorRef,pks:PublicKeys,box:Box)
case class SendTx(transaction:Transaction,box:Box)

//messages between coordinator and holders
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

