package prosomo.stakeholder

import akka.actor.{ActorPath, ActorRef, Props}
import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{BlockStorage, Chain, ChainStorage, Serializer, SlotReorgHistory}
import prosomo.history.History
import prosomo.primitives.{Kes, KeyFile, Keys, Parameters, Sig, Vrf}
import prosomo.wallet.{Wallet, WalletStorage}

import scala.math.BigInt
import scala.util.Random

/**
  * Stakeholder actor that executes the staking protocol and communicates with other stakeholders,
  * sends the coordinator the public key upon instantiation and gets the genesis block from coordinator
  */

class Stakeholder(inputSeed:Array[Byte])
  extends ChainSelection
  with Forging
  with Ledger
  with Messages
  with Operations
  with Receive
  with Staking
  with Transactions
  with Update
  with Utilities
  with Validation
{
  import Parameters.{dataFileDir,fee_r}
  val seed:Array[Byte] = inputSeed
  val serializer:Serializer = new Serializer
  val storageDir:String = dataFileDir+self.path.toStringWithoutAddress.drop(5)
  val localChain:Chain = new Chain
  val blocks:BlockStorage = new BlockStorage(storageDir)
  val chainHistory:SlotReorgHistory = new SlotReorgHistory(storageDir)
  val chainStorage = new ChainStorage(storageDir)
  val walletStorage = new WalletStorage(storageDir)
  val vrf = new Vrf
  val kes = new Kes
  val sig = new Sig
  val rng:Random = new Random(BigInt(seed).toLong)
  var keys:Keys = Keys(seed,sig,vrf,kes,0)
  var wallet:Wallet = new Wallet(keys.pkw,fee_r)
  val history:History = new History(storageDir)
  val holderId:ActorPath = self.path
  val sessionId:Sid = ByteArrayWrapper(FastCryptographicHash(holderId.toString))
  val phase:Double = rng.nextDouble
  //stakeholder password, set at runtime
  var password = ""
  var derivedKey:Array[Byte] = Array()
  var salt:Array[Byte] = Array()
  //empty keyfile, doesn't write anything to disk
  var keyFile:KeyFile = KeyFile.empty
  var chainUpdateLock = false
  var routerRef:ActorRef = _
  var localState:State = Map()
  var eta:Eta = Array()
  var stakingState:State = Map()
  var memPool:MemPool = Map()
  var holderIndex:Int = -1
  var diffuseSent = false
  //list of all or some of the stakeholders, including self, that the stakeholder is aware of
  var holders: List[ActorRef] = List()
  //list of stakeholders that all new blocks and transactions are sent to
  var gossipers: List[ActorRef] = List()
  //gossipers offset
  var gOff = 0
  //number of tries to issue hello in slots
  var numHello = 0
  //map of all session IDs and public keys associated with holders in holder list
  var inbox:Map[Sid,(ActorRef,PublicKeys)] = Map()
  //total number of times this stakeholder was elected slot leader
  var blocksForged = 0
  //slot time as determined from coordinator clock
  var globalSlot = 0
  //all tines that are pending built from new blocks that are received
  var tines:Map[Int,(Chain,Int,Int,Int,ActorRef)] = Map()
  //counter for identifying tines
  var tineCounter = 0
  //completed tines waiting to be selected with maxvalid-bg
  var candidateTines:Array[(Chain,Slot,Int)] = Array()
  //placeholder for genesis block
  var genBlockHeader: BlockHeader = _
  //placeholder for genesis block ID
  var genBlockHash: Hash = ByteArrayWrapper(Array())
  //placeholder for forged block if elected slot leader
  var roundBlock: Int = 0
  //max time steps set by coordinator
  var tMax = 0
  //start system time set by coordinator
  var t0:Long = 0
  //current slot that is being processed by stakeholder
  var localSlot = 0
  //current epoch that is being processed by stakeholder
  var currentEpoch = -1
  //lock for update message
  var updating = false
  //lock for stalling stakeholder
  var actorStalled = false
  //ref of coordinator actor
  var coordinatorRef:ActorRef = _
  //total number of transactions issued
  var txCounter = 0
  //set of all txs issued by holder
  var setOfTxs:Map[Sid,Int] = Map()
  //toggle if holder is adversary
  var adversary:Boolean = false
  //toggle for covert mining
  var covert:Boolean = false
  //toggle for nothing-at-stake forging
  var forgeAll:Boolean = false
}

object Stakeholder {
  def props(seed:Array[Byte]): Props = Props(new Stakeholder(seed))
}

