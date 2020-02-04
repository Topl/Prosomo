package prosomo

import akka.actor.ActorRef
import io.iohk.iodb.ByteArrayWrapper

trait StakeholderVariables
  extends Types
    with Methods
    with Utils {
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
  var genBlock: Any = 0
  //placeholder for genesis block ID
  var genBlockHash: Hash = ByteArrayWrapper(Array())
  //placeholder for forged block if elected slot leader
  var roundBlock: Any = 0
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

