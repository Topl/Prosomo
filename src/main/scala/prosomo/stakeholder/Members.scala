package prosomo.stakeholder

import akka.actor.{Actor, ActorPath, ActorRef, Timers}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives.{Kes, Keys, Ratio, Sig, Vrf}
import prosomo.components.{Block, BlockData, Box, Chain, ChainStorage, Serializer, SimpleTypes, SlotReorgHistory, Transaction}
import prosomo.history.History
import prosomo.wallet.Wallet
import scala.math.BigInt
import scala.util.Random

trait Members extends SimpleTypes with Actor with Timers {

  val seed:Array[Byte]
  val serializer:Serializer
  val storageDir:String
  val localChain:Chain
  val blocks:BlockData
  val chainHistory:SlotReorgHistory
  val vrf:Vrf
  val kes:Kes
  val sig:Sig
  val history:History
  val rng:Random
  val holderId:ActorPath
  val sessionId:Sid
  val phase:Double
  val keys:Keys
  val wallet:Wallet
  val chainStorage:ChainStorage

  var localState:State
  var eta:Eta
  var stakingState:State
  var memPool:MemPool
  var holderIndex:Int
  var diffuseSent:Boolean
  var routerRef:ActorRef
  var chainUpdateLock:Boolean
  var holders: List[ActorRef]
  var gossipers: List[ActorRef]
  var gOff:Int
  var numHello:Int
  var inbox:Map[Sid,(ActorRef,PublicKeys)]
  var blocksForged:Int
  var globalSlot:Slot
  var tines:Map[Int,(Chain,Int,Int,Int,ActorRef)]
  var tineCounter:Int
  var candidateTines:Array[(Chain,Slot,Int)]
  var genBlockHeader:BlockHeader
  var genBlockHash:Hash
  var roundBlock:Any
  var tMax:Int
  var t0:Long
  var localSlot:Slot
  var currentEpoch:Int
  var updating:Boolean
  var actorStalled:Boolean
  var coordinatorRef:ActorRef
  var txCounter:Int
  var setOfTxs:Map[Sid,Int]
  var adversary:Boolean
  var covert:Boolean
  var forgeAll:Boolean

  case object timerKey

  def forgeBlock(forgerKeys:Keys):Unit
  def updateTine(inputTine:Chain):(Chain,Slot)
  def updateWallet:Unit
  def buildTine(job:(Int,(Chain,Int,Int,Int,ActorRef))):Unit
  def maxValidBG:Unit
  def validateChainIds(c:Chain):Unit
  def updateSlot:Unit
  def updateEpoch(slot:Slot,epochIn:Int):Int
  def updateStakingState(ep:Int):Unit
  def update:Unit
  def hash(input:ActorRef,serializer: Serializer): Hash
  def hash(input:Slot,serializer: Serializer):Hash
  def hash(input:(ActorRef,PublicKeys),serializer: Serializer):Hash
  def hashGenEntry(input:(Array[Byte], ByteArrayWrapper, BigInt),serializer: Serializer):Hash
  def hash(input:BlockHeader,serializer: Serializer):Hash
  def hash(input:Transaction,serializer: Serializer):Hash
  def hash(input:(List[Hash],Int,Int),serializer: Serializer):Hash
  def hashGen(input:GenesisSet,serializer: Serializer):Hash
  def hash(input:TransactionSet,serializer: Serializer):Hash
  def hash(input:String,serializer: Serializer):Hash
  def verifyTX(transaction: Transaction,sig:Sig,serializer: Serializer):Boolean
  def applyTransaction(t: Transaction,ls:State, forger:PublicKeyW, fee_r:Ratio):Any
  def getParentId(b:BlockHeader):SlotId
  def lastActiveSlot(c:Chain, s:Slot):Slot
  def getActiveSlots(c:Chain):Int
  def subChain(c:Chain, t1:Int, t2:Int):Chain
  def phi(a:Ratio):Ratio
  def factorial(n: Int):Int
  def compare(y: Array[Byte],t: Ratio):Boolean
  def relativeStake(holderKey:PublicKeyW,ls:State):Ratio
  def uuid:String
  def bytes2hex(b: Array[Byte]):String
  def hex2bytes(hex: String): Array[Byte]
  def containsDuplicates(s:Map[String,String]):Boolean
  def getBlockHeader(bid:SlotId):Any
  def getParentBlockHeader(b:BlockHeader):Any
  def getParentId(bid:SlotId):Any
  def eta(c:Chain, ep:Int):Eta
  def eta(c:Chain, ep:Int, etaP:Eta):Eta
  def diffuse(str: String,id: String,sk_sig: PrivateKey):String
  def signBox(data: Hash, id:Sid, sk_sig: PrivateKey, pk_sig: PublicKey):Box
  def verifyBox(input:Hash,box:Box):Boolean
  def gossipSet(id:ActorPath,h:List[ActorRef]):List[ActorRef]
  def send(sender:ActorRef,holder:ActorRef,command: Any):Unit
  def send(sender:ActorRef,holders:List[ActorRef],command: Any):Unit
  def sendAssertDone(holders:List[ActorRef], command: Any):Unit
  def sendAssertDone(holder:ActorRef, command: Any):Unit
  def getGossipers(holders:List[ActorRef]):Map[ActorRef,List[ActorRef]]
  def getStakingState(holder:ActorRef):State
  def getBlockTree(holder:ActorRef):Unit
  def getPositionData(router:ActorRef):(Map[ActorRef,(Double,Double)],Map[(ActorRef,ActorRef),Long])
  def collectKeys(holders:List[ActorRef], command: Any, input: Map[String,String]): Map[String,String]
  def sendDiffuse(holderId:ActorPath, holders:List[ActorRef], command: Any):Unit
  def verifyBlockHeader(b:BlockHeader):Boolean
  def verifyBlock(b:Block):Boolean
  def verifyChain(c:Chain, gh:Hash):Boolean
  def verifySubChain(tine:Chain, prefix:Slot):Boolean
  def verifyTransaction(t:Transaction):Boolean
  def updateLocalState(ls:State, c:Chain):Any
  def trimMemPool:Unit
  def collectLedger(c:Chain):Unit
  def chooseLedger(pkw:PublicKeyW,mp:MemPool,s:State):TransactionSet
  def verifyStamp(value: String):Boolean
  def timeFlag[R](block: => R):R
  def time[R](block: => R):R

}