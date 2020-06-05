package prosomo.stakeholder

import akka.actor.{ActorPath, Cancellable, Props}
import com.google.common.cache.LoadingCache
import prosomo.primitives.{Fch, Kes, KeyFile, Keys, Parameters, Ratio, Sig, Vrf}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Serializer, Tine, Wallet}
import prosomo.history.{BlockStorage, ChainStorage, StateStorage, WalletStorage}

import scala.math.BigInt
import scala.util.Random

/**
  * Stakeholder actor that executes the staking procedure and participates in consensus,
  * Each stakeholder actor represents a distinct node view with different modifiers in their pools and databases,
  * This is the primary node view holder,
  * Should only communicate with local and remote interfaces (Coordinator and Router respectively)
  * @param inputSeed input entropy
  * @param holderIndex an integer index for identifying executing actors in thread locks
  * @param inputRef network controller refs and router ref
  */

class Stakeholder(
                   inputSeed:Array[Byte],
                   override val holderIndex:Int,
                   inputRef:Seq[ActorRefWrapper],
                   inputKeyFile:Option[KeyFile],
                   inputDataDir:Option[String],
                   inputPassword:Option[String],
                   inputKeyDir:Option[String]
                 )
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
  implicit val routerRef:ActorRefWrapper = inputRef(0)
  val seed:Array[Byte] = inputSeed
  val serializer:Serializer = new Serializer
  val storageDir:String = inputDataDir match {
    case None => dataFileDir+self.path.toStringWithoutAddress.drop(5)
    case Some(dir) => dir
  }
  val localChain:Tine = new Tine
  val blocks:BlockStorage = new BlockStorage(storageDir,serializer)
  val chainStorage = new ChainStorage(storageDir)
  val walletStorage = new WalletStorage(storageDir)
  val vrf = new Vrf
  val kes = new Kes
  val sig = new Sig
  override val fch = new Fch
  val rng:Random = new Random(BigInt(seed).toLong)
  var keys:Keys = Keys(seed,sig,vrf,kes,0)
  var wallet:Wallet = Wallet(keys.pkw,fee_r)
  val history:StateStorage = new StateStorage(storageDir,serializer)
  val holderId:ActorPath = self.path
  val sessionId:Sid = ByteArrayWrapper(fch.hash(holderId.toString))
  val phase:Double = rng.nextDouble
  val selfWrapper:ActorRefWrapper = ActorRefWrapper(self)
  //stakeholder password, set at runtime, for research runs with deterministic entropy
  var password = inputPassword match {
    case Some(str)=>str
    case None => ""
  }
  var derivedKey:Array[Byte] = Array()
  var salt:Array[Byte] = Array()
  var keyFile:Option[KeyFile] = inputKeyFile
  var keyDir = inputKeyDir match {
    case None => storageDir+"/keys/"
    case Some(dir) => dir
  }
  var chainUpdateLock = false
  var localState:State = Map()
  var eta:Eta = Array()
  var stakingState:State = Map()
  var memPool:MemPool = Map()
  var diffuseSent = false
  //list of all or some of the stakeholders, including self, that the stakeholder is aware of
  var holders: List[ActorRefWrapper] = List()
  //list of stakeholders that all new blocks and transactions are sent to
  var gossipers: List[ActorRefWrapper] = List()
  //gossipers offset
  var gOff = 0
  //number of tries to issue hello in slots
  var numHello = 0
  //map of all session IDs and public keys associated with holders in holder list
  var inbox:Map[Sid,(ActorRefWrapper,PublicKeys)] = Map()
  //total number of times this stakeholder was elected slot leader
  var blocksForged = 0
  //slot time as determined from coordinator clock
  var globalSlot = 0
  //all tines that are pending built from new blocks that are received
  var tinePool:Map[Int,(Tine,Int,Int,Int,ActorRefWrapper)] = Map()
  //counter for identifying tines
  var tineCounter = 0
  //completed tines waiting to be selected with maxvalid-bg
  var tinePoolWithPrefix:Array[(Tine,Slot,Int)] = Array()
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
  var currentEpoch = 0
  //lock for update message
  var updating = false
  //lock for stalling stakeholder
  var actorStalled = false
  //ref of coordinator actor
  var coordinatorRef:ActorRefWrapper = _
  //total number of transactions issued
  var txCounter = 0
  //toggle if holder is adversary
  var adversary:Boolean = false
  //toggle for covert mining
  var covert:Boolean = false
  //toggle for nothing-at-stake forging
  var forgeAll:Boolean = false

  var bootStrapLock:Boolean = false
  var bootStrapJob:Int = -1
  var bootStrapMessage: Cancellable = _
  var tineProvider:Option[ActorRefWrapper] = None
  var alphaCache: Option[LoadingCache[ByteArrayWrapper, Ratio]] = None
  var thresholdCache: Option[LoadingCache[(Ratio,Slot), Ratio]] = None
}

object Stakeholder {
  def props(seed:Array[Byte],index:Int,ref:Seq[akka.actor.ActorRef]): Props =
    Props(new Stakeholder(seed,index,ref.map(ActorRefWrapper(_)(ActorRefWrapper.routerRef(ref(0)))),None,None,None,None))
  def props(seed:Array[Byte],index:Int,ref:Seq[akka.actor.ActorRef],keyFile: KeyFile,dir:String,password:String,kdir:String): Props =
    Props(new Stakeholder(seed,index,ref.map(ActorRefWrapper(_)(ActorRefWrapper.routerRef(ref(0)))),Some(keyFile),Some(dir),Some(password),Some(kdir)))
}

