package prosomo.stakeholder

import akka.actor.{Actor, ActorPath, Cancellable, Timers}
import com.google.common.cache.LoadingCache
import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives.{Fch, Kes, KeyFile, Keys, Mac, Ratio, Sig, SimpleTypes, Vrf}
import prosomo.components.{Block, Serializer, Tine, Transaction, Wallet}
import prosomo.history.{BlockStorage, ChainStorage, StateStorage, WalletStorage}
import scala.math.BigInt
import scala.util.Random

trait Members extends SimpleTypes with Actor with Timers {

  implicit val routerRef:ActorRefWrapper
  val holderIndex:Int
  val seed:Array[Byte]
  val serializer:Serializer
  val storageDir:String
  val localChain:Tine
  val blocks:BlockStorage
  val walletStorage:WalletStorage
  val vrf:Vrf
  val kes:Kes
  val sig:Sig
  val fch:Fch
  val history:StateStorage
  val rng:Random
  val holderId:ActorPath
  val sessionId:Sid
  val phase:Double
  val chainStorage:ChainStorage
  val selfWrapper:ActorRefWrapper

  var password:String
  var derivedKey:Array[Byte]
  var salt:Array[Byte]
  var wallet:Wallet
  var keys:Keys
  var keyFile:KeyFile
  var localState:State
  var eta:Eta
  var stakingState:State
  var memPool:MemPool
  var chainUpdateLock:Boolean
  var holders: List[ActorRefWrapper]
  var gossipers: List[ActorRefWrapper]
  var gOff:Int
  var numHello:Int
  var inbox:Map[Sid,(ActorRefWrapper,PublicKeys)]
  var blocksForged:Int
  var globalSlot:Slot
  var tines:Map[Int,(Tine,Int,Int,Int,ActorRefWrapper)]
  var tineCounter:Int
  var candidateTines:Array[(Tine,Slot,Int)]
  var genBlockHeader:BlockHeader
  var genBlockHash:Hash
  var roundBlock:Int
  var t0:Long
  var localSlot:Slot
  var currentEpoch:Int
  var updating:Boolean
  var actorStalled:Boolean
  var coordinatorRef:ActorRefWrapper
  var txCounter:Int
  var adversary:Boolean
  var covert:Boolean
  var forgeAll:Boolean

  var bootStrapLock:Boolean
  var bootStrapJob:Int
  var bootStrapMessage:Cancellable
  var tineProvider:Option[ActorRefWrapper]
  var alphaCache:Option[LoadingCache[ByteArrayWrapper,Ratio]]
  var thresholdCache:Option[LoadingCache[(Ratio,Slot),Ratio]]

  case object TimerKey

  def forgeBlock(forgerKeys:Keys):Unit
  def updateTine(inputTine:Tine):Option[(Tine,Slot)]
  def updateWallet:Unit
  def buildTine(job:(Int,(Tine,Int,Int,Int,ActorRefWrapper))):Unit
  def maxValidBG:Unit
  def validateChainIds(c:Tine):Boolean
  def updateEpoch(slot:Slot,epochIn:Int,lastEta:Eta,chain:Tine):(Int,Eta)
  def getStakingState(ep:Int,chain:Tine):State
  def update:Unit
  def hash(input:ActorRefWrapper, serializer: Serializer): Hash
  def hash(input:Slot,serializer: Serializer):Hash
  def hash(input:(ActorRefWrapper,PublicKeys), serializer: Serializer):Hash
  def hashGenEntry(input:(Array[Byte], ByteArrayWrapper, BigInt),serializer: Serializer):Hash
  def hash(input:BlockHeader,serializer: Serializer):Hash
  def hash(input:Transaction,serializer: Serializer):Hash
  def hash(input:(List[SlotId],Int,Int),serializer: Serializer):Hash
  def hashGen(input:GenesisSet,serializer: Serializer):Hash
  def hash(input:TransactionSet,serializer: Serializer):Hash
  def hash(input:String,serializer: Serializer):Hash
  def verifyTX(transaction: Transaction,sig:Sig,serializer: Serializer):Boolean
  def applyTransaction(t: Transaction,ls:State, forger:PublicKeyW, fee_r:Ratio):Option[State]
  def getParentId(b:BlockHeader):SlotId
  def lastActiveSlot(c:Tine, s:Slot):Slot
  def getActiveSlots(c:Tine):Int
  def subChain(c:Tine, t1:Int, t2:Int):Tine
  def phi(a:Ratio):Ratio
  def phi(a:Ratio,m_f:Ratio):Ratio
  def threshold_cached(a:Ratio, s_interval:Slot):Ratio
  def threshold(a:Ratio, s_interval:Slot):Ratio
  def factorial(n: Int):BigInt
  def compare(y: Array[Byte],t: Ratio):Boolean
  def relativeStake(holderKey:PublicKeyW,ls:State):Ratio
  def uuid:String
  def bytes2hex(b: Array[Byte]):String
  def hex2bytes(hex: String): Array[Byte]
  def containsDuplicates(s:Map[String,String]):Boolean
  def getBlockHeader(bid:SlotId):Option[BlockHeader]
  def getParentBlockHeader(b:BlockHeader):Option[BlockHeader]
  def getParentId(bid:SlotId):Option[SlotId]
  def getNonce(id:SlotId):Option[Rho]
  def eta_from_genesis(c:Tine, ep:Int):Eta
  def eta_from_tine(c:Tine, ep:Int, eta_prev:Eta):Eta
  def signMac(data: Hash, id:Sid, sk_sig: PrivateKey, pk_sig: PublicKey):Mac
  def verifyMac(input:Hash, mac:Mac):Boolean
  def gossipSet(id:ActorPath,h:List[ActorRefWrapper]):List[ActorRefWrapper]
  def send(sender:ActorRefWrapper, holder:ActorRefWrapper, command: Any):Unit
  def send(sender:ActorRefWrapper, holders:List[ActorRefWrapper], command: Any):Unit
  def sendAssertDone(holders:List[ActorRefWrapper], command: Any):Unit
  def sendAssertDone(holder:ActorRefWrapper, command: Any):Unit
  def getGossipers(holders:List[ActorRefWrapper]):Map[ActorRefWrapper,List[ActorRefWrapper]]
  def getStakingState(holder:ActorRefWrapper):State
  def getBlockTree(holder:ActorRefWrapper):Unit
  def getPositionData(router:ActorRefWrapper):(Map[ActorRefWrapper,(Double,Double)],Map[(ActorRefWrapper,ActorRefWrapper),Long])
  def collectKeys(holders:List[ActorRefWrapper], command: Any, input: Map[String,String]): Map[String,String]
  def verifyBlockHeader(b:BlockHeader):Boolean
  def verifyBlock(b:Block):Boolean
  def verifyChain(c:Tine, gh:Hash):Boolean
  def verifySubChain(tine:Tine, prefix:Slot):Boolean
  def verifyTransaction(t:Transaction):Boolean
  def updateLocalState(ls:State, c:Tine):Option[State]
  def updateLocalState(ls:State, id:SlotId):Option[State]
  def trimMemPool:Unit
  def collectLedger(c:Tine):Unit
  def chooseLedger(pkw:PublicKeyW,mp:MemPool,s:State):TransactionSet
  def verifyStamp(value: String):Boolean
  def timeFlag[R](block: => R):R
  def time[R](block: => R):R

}