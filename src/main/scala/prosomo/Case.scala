package prosomo

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
case class NullBlock(job:Int)
case class GetSlot(s:Int)
case class Hello(id: Any)
case class CoordRef(ref: Any)
case class RouterRef(ref: Any)
case class GetTime(t1:Long)
case class Initialize(tMax:Int)
case class SetClock(t0:Long)
case class GenBlock(b: Any)
case class SendBlock(s:Any)
case class RequestBlock(s:Any)
case class RequestChain(s:Any)
case class ReturnBlock(s:Any)
case class SendTx(s:Any)
case class IssueTx(s:Any)
case class WriteFile(fw: Any)
case class NewGraphFile(name:String)
case class GetGossipers(list:Any)
case class Party(list:Any,clear:Boolean)
case class GetState(s:Any)
case class GetBlockTree(t:Any,h:Any)
case class GetPositionData(s:Any)
case class Adversary(s:String)

