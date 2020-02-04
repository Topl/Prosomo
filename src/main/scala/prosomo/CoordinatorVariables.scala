package prosomo

import akka.actor.ActorRef
import bifrost.crypto.hash.FastCryptographicHash

trait CoordinatorVariables
  extends Types
    with Methods
    with Utils {
  //empty list of stake holders
  var holders: List[ActorRef] = List()
  //list of parties
  var parties: List[List[ActorRef]] = List()
  //holder keys for genesis block creation
  var holderKeys:Map[ActorRef,PublicKeyW] = Map()
  //slot
  var t:Slot = 0
  //initial system time
  var t0:Long = 0
  //system time paused offset
  var tp:Long = 0
  //lock for stalling coordinator
  var actorStalled = false
  //lock for pausing system
  var actorPaused = false
  //queue of commands to be processed in a given slot
  var cmdQueue:Map[Slot,List[String]] = inputCommands
  //set of keys so genesis block can be signed and verified by verifyBlock
  val seed:Array[Byte] = FastCryptographicHash(inputSeed+"seed")
  //initial nonce for genesis block
  val eta0:Eta = FastCryptographicHash(inputSeed+"eta0")
  val (sk_sig,pk_sig) = sig.createKeyPair(seed)
  val (sk_vrf,pk_vrf) = vrf.vrfKeypair(seed)
  var sk_kes = kes.generateKey(seed)
  val pk_kes:PublicKey = kes.publicKey(sk_kes)

  val coordData:String = bytes2hex(pk_sig)+":"+bytes2hex(pk_vrf)+":"+bytes2hex(pk_kes)
  val coordKeys:PublicKeys = (pk_sig,pk_vrf,pk_kes)
  //empty list of keys to be populated by stakeholders once they are instantiated
  var genKeys:Map[String,String] = Map()
  var fileWriter:Any = 0
  var graphWriter:Any = 0
  var gossipersMap:Map[ActorRef,List[ActorRef]] = Map()
  var transactionCounter:Int = 0
}
