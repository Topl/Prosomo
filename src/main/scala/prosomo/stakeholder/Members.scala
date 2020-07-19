package prosomo.stakeholder

import akka.actor.{Actor, ActorPath, Timers}
import com.google.common.cache.LoadingCache
import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives.{ActorRefWrapper, Fch, Kes, KeyFile, Keys, Parameters, Ratio, Sig, SimpleTypes, Vrf}
import prosomo.components.{Block, Serializer, Tine, Transaction, Wallet}
import prosomo.history.{BlockStorage, ChainStorage, StateStorage, WalletStorage}

import scala.concurrent.duration._
import scala.math.BigInt
import scala.util.Random

/**
  * AMS 2020:
  * The root trait for actors that will participate in consensus
  * All members required to execute Ouroboros routines and maintain state, mempool, tinepool, and wallet
  */

trait Members extends SimpleTypes with Actor with Timers {

  implicit val routerRef:ActorRefWrapper
  implicit val blocks:BlockStorage

  val devMode:Boolean = Parameters.devMode
  var useGui:Boolean = Parameters.useGui
  val timeScale:Double = Parameters.timeScale
  val dataBaseCID: ByteArrayWrapper = Parameters.dataBaseCID
  val genesisBytes: ByteArrayWrapper = Parameters.genesisBytes
  val inputSeedString:String = Parameters.inputSeedString
  //use network delay parameterization if true
  val useDelayParam:Boolean = Parameters.useDelayParam
  //number of stakeholders
  val numGenesisHolders:Int = Parameters.numGenesisHolders
  //minumum index of local holders, set to -1 for all local
  val holderIndexMin:Int = Parameters.holderIndexMin
  //maximum index of local holders, set to -1 for all local
  val holderIndexMax:Int = Parameters.holderIndexMax
  //duration of slot in milliseconds
  val slotT:Long = Parameters.slotT
  //interval between return blocks in tine provider
  val requestTineInterval:Int = Parameters.requestTineInterval
  //delay in milliseconds per kilometer in router model
  val delay_ms_km:Double = Parameters.delay_ms_km
  //delay in milliseconds per bit in router model
  val delay_ms_byte:Double = Parameters.delay_ms_byte
  //network random noise max
  val delay_ms_noise:Double = Parameters.delay_ms_noise
  //communication method
  val useRouting:Boolean = Parameters.useRouting
  //delay in slots, calculated as maximum possible delay in random global network model
  val delta_s:Int = Parameters.delta_s
  //epoch parameter
  val epsilon_s:Double = Parameters.epsilon_s
  //alert stake ratio
  val alpha_s:Double = Parameters.alpha_s
  //participating stake ratio
  val beta_s:Double = Parameters.beta_s
  //active slot coefficient
  val f_s:Double = Parameters.f_s
  //order of accuracy for convergent series
  val o_n:Int = Parameters.o_n
  val m_f_root:Ratio = Parameters.m_f_root
  val f_dynamic:Boolean = Parameters.f_dynamic
  val testStrategy:String = Parameters.testStrategy
  val f_min:Ratio = Parameters.f_min
  val f_max:Ratio = Parameters.f_max
  val num_f:Int = Parameters.num_f
  val m_f_range:Array[Ratio] = Parameters.m_f_range
  val k_s:Int = Parameters.k_s
  // epoch length R >= 3k/2f
  val epochLength:Int = Parameters.epochLength
  val one_third_epoch:Int = Parameters.one_third_epoch
  // slot window for chain selection, s = k/4f
  val slotWindow:Int = Parameters.slotWindow
  //status and verify check chain hash data up to this depth to gauge consensus amongst actors
  val confirmationDepth:Int = Parameters.confirmationDepth
  //max initial stake
  val initStakeMax:Double = Parameters.initStakeMax
  //max random transaction delta
  val maxTransfer:Double = Parameters.maxTransfer
  //reward for forging blocks
  val forgerReward:BigInt = Parameters.forgerReward
  //percent of transaction amount taken as fee by the forger
  val transactionFee:Double = Parameters.transactionFee
  val fee_r:Ratio = Parameters.fee_r
  //number of holders on gossip list for sending new blocks and transactions
  val numGossipers:Int = Parameters.numGossipers
  //number of holders to gossip to upon forging
  val numGossipersForge:Int = Parameters.numGossipersForge
  //max number of tries for a tine to ask for parent blocks
  val tineMaxTries:Int = Parameters.tineMaxTries
  //max depth in multiples of confirmation depth that can be returned from an actor
  val tineMaxDepth:Int = Parameters.tineMaxDepth
  //data write interval in slots
  val dataOutInterval:Int = Parameters.dataOutInterval
  //time out for dropped messages from coordinator
  val waitTime:FiniteDuration = Parameters.waitTime
  //duration between update tics that stakeholder actors send to themselves
  val updateTime:FiniteDuration = Parameters.updateTime
  //duration between command read tics and transaction generation for the coordinator
  val commandUpdateTime:FiniteDuration = Parameters.commandUpdateTime
  //number of txs per block
  val txPerBlock:Int = Parameters.txPerBlock
  //Issue random transactions if true
  var transactionFlag:Boolean = Parameters.transactionFlag
  // p = txProbability => (1-p)^numHolders
  var txProbability:Double = Parameters.txProbability
  //uses randomness for public key seed and initial stake, set to false for deterministic run
  val randomFlag:Boolean = Parameters.randomFlag
  //when true, if system cpu load is too high the coordinator will stall to allow stakeholders to catch up
  val performanceFlag:Boolean = Parameters.performanceFlag
  //threshold of cpu usage above which coordinator will stall if performanceFlag = true
  val systemLoadThreshold:Double = Parameters.systemLoadThreshold
  //number of values to average for load threshold
  val numAverageLoad:Int = Parameters.numAverageLoad
  //print Stakeholder 0 status per slot if true
  val printFlag:Boolean = Parameters.printFlag
  //print Stakeholder 0 execution time per slot if true
  val timingFlag:Boolean = Parameters.timingFlag
  //Record data if true, plot data points with ./cmd.sh and enter command: plot
  val dataOutFlag:Boolean = Parameters.dataOutFlag
  //toggle for action based round execution
  val useFencing:Boolean = Parameters.useFencing
  //interval in slots between localChain save to disk
  val chainStoreInterval:Int = Parameters.chainStoreInterval
  //number of message processors
  val numMessageProcessors:Int = Parameters.numMessageProcessors
  //node secret for HMAC used in each router actor
  val sk_ecx:Array[Byte] = Parameters.sk_ecx
  //public key for HMAC used in each router actor
  val pk_ecx:Array[Byte] = Parameters.pk_ecx

  //path for data output files
  val dataFileDir:String = Parameters.dataFileDir
  val storageFlag:Boolean = Parameters.storageFlag
  val cacheSize:Int = Parameters.cacheSize
  val refreshInterval:Int = Parameters.refreshInterval
  val stakeDistribution:String = Parameters.stakeDistribution
  val stakeScale:Double = Parameters.stakeScale
  val initStakeMin:Double = Parameters.initStakeMin
  val timeServer:String = Parameters.timeServer

  val localRef:ActorRefWrapper
  val holderIndex:Int
  val seed:Array[Byte]
  val serializer:Serializer
  val storageDir:String
  val localChain:Tine
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
  var keyDir:String
  var wallet:Wallet
  var keys:Keys
  var keyFile:Option[KeyFile]
  var localState:State
  var eta:Eta
  var stakingState:State
  var memPool:MemPool
  var chainUpdateLock:Boolean
  var holders: List[ActorRefWrapper]
  var gOff:Int
  var numHello:Int
  var inbox:Map[Sid,(ActorRefWrapper,PublicKeys)]
  var blocksForged:Int
  var globalSlot:Slot
  var tinePool:Map[Int,(Tine,Int,Int,Int,ActorRefWrapper)]
  var tinePoolWithPrefix:Array[(Tine,Slot,Int)]
  var tineCounter:Int
  var genBlockHeader:BlockHeader
  var genBlockHash:Hash
  var roundBlock:Int
  var t0:Long
  var t1:Long
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
  var helloLock:Boolean
  var bootStrapJob:Int
  var tineProvider:Option[ActorRefWrapper]
  var alphaCache:Option[LoadingCache[ByteArrayWrapper,Ratio]]
  var thresholdCache:Option[LoadingCache[(Ratio,Slot),Ratio]]
  var networkDelayList:List[Double]
  var genesisBlock:Option[Block]

  def forgeBlock(forgerKeys:Keys):Unit
  def updateTine(inputTine:Tine):Option[(Tine,Slot)]
  def updateWallet():Unit
  def buildTine(job:(Int,(Tine,Int,Int,Int,ActorRefWrapper))):Unit
  def maxValidBG():Unit
  def bootstrapAdoptTine():Unit
  def updateEpoch(slot:Slot,epochIn:Int,lastEta:Eta,chain:Tine,tine:Option[Tine]):(Int,Eta)
  def getStakingState(ep:Int,chain:Tine,tine:Option[Tine]):State
  def stakingTestStrategy(y:Rho,ps:Slot,bn:Int):Rho
  def update():Unit
  def scheduleDiffuse():Unit
  def Sha512(bytes: Array[Byte]):Array[Byte]
  def hash(input:ActorRefWrapper,slot:Slot, serializer: Serializer): Hash
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
  def getBlockHeader(bid:SlotId):Option[BlockHeader]
  def getParentBlockHeader(b:BlockHeader):Option[BlockHeader]
  def getParentId(bid:SlotId):Option[SlotId]
  def getNonce(id:SlotId):Option[Rho]
  def eta_from_genesis(c:Tine, ep:Int):Eta
  def eta_from_tine(c:Tine,ep:Int,eta_prev:Eta,tine:Option[Tine]):Eta
  def gossipSet(self:ActorRefWrapper,holders:List[ActorRefWrapper]):List[ActorRefWrapper]
  def gossipSet(self:ActorRefWrapper,sender:ActorRefWrapper,holders:List[ActorRefWrapper]):List[ActorRefWrapper]
  def send(sender:ActorRefWrapper, holder:ActorRefWrapper, command: Any):Unit
  def send(sender:ActorRefWrapper, holders:List[ActorRefWrapper], command: Any):Unit
  def sendAssertDone(holders:List[ActorRefWrapper], command: Any):Unit
  def sendAssertDone(holder:ActorRefWrapper, command: Any):Unit
  def getStakingState(holder:ActorRefWrapper):State
  def blockTree(holder:ActorRefWrapper):Unit
  def getPositionData(router:ActorRefWrapper):(Map[ActorRefWrapper,(Double,Double)],Map[(ActorRefWrapper,ActorRefWrapper),Long])
  def verifyBlockHeader(b:BlockHeader):Boolean
  def verifyBlock(b:Block):Boolean
  def verifyChain(c:Tine, gh:Hash):Boolean
  def verifyTine(tine:Tine, prefix:Slot):Boolean
  def verifyTransaction(t:Transaction):Boolean
  def updateLocalState(ls:State, c:Tine):Option[State]
  def updateLocalState(ls:State, id:SlotId):Option[State]
  def trimMemPool():Unit
  def collectLedger(c:Tine):Unit
  def chooseLedger(pkw:PublicKeyW,mp:MemPool,s:State):TransactionSet
  def timeFlag[R](block: => R):R
  def time[R](block: => R):R

}