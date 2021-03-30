package prosomo

import com.google.common.primitives.Ints
import prosomo.primitives.Ratio

import java.security.MessageDigest
import scala.collection.mutable
import scala.math.BigInt
import scala.util.Random

/**
 * AMS 2021: Conditional Settlement Game Simulator
 * This represents a simple toy model of adversarial behavior in the execution of Ouroboros.
 * The adaptive adversary using local-dynamic-difficulty is modeled with a set of automata that behave according
 * to a longest chain selection rule.  Idealized representations of the leadership election mechanism and selection rule
 * are implemented with the intention of numerical efficiency.  This provides a performant test bed
 * to explore the nature of the adaptive adversary with a conditional leader election process.
 * Block ids are represented with randomly generated integers and the structure of tines can be constructed from the
 * labels (slots, nonces)
 */

class SettlementGameSim{

  //print as the simulation executes
  val printGame = true
  //total number of automata
  val numHolders = 100
  //total number of players
  val numAdversary = 1
  //total number of challengers
  val numHonest:Int = numHolders - numAdversary
  //proportion of adversarial stake
  val alpha:Double = 0.4
  //number of rounds to be executed
  val T = 10000
  //generates random IDs for blocks
  val rnd:Random = new Random
  //epoch nonce in VRF tests
  val seed:Array[Byte] = Array.fill(32){0x00.toByte}
  rnd.nextBytes(seed)
  //map of slots that have more than one block
  val forkedSlots:mutable.Map[Int,Set[Int]] = mutable.Map.empty
  //map of slots that have one block
  val uniqueSlots:mutable.Map[Int,Set[Int]] = mutable.Map.empty
  //forging window cutoff
  val gamma = 40
  //slot gap
  val psi = 0
  //max difficulty
  val f_A = 0.4
  //base difficulty
  val f_B = 0.05

  //database of all blocks, begins with genesis block, key is block id
  val blockDb:mutable.Map[Int,Block] = mutable.Map(0 -> Block(0,0,0,0,0,0))
  // Delta, the total delay in slots in the bounded delay semi-synchronous setting
  val bounded_delay = 10
  // Tracks honest blocks for delivery after set delay of bounded_delay slots, delay decrements with time step
  // Key is block id, value is delay that decrements with each time step
  val deliverySchedule:mutable.Map[Int,Int] = mutable.Map.empty
  // Threshold cache for adversary
  var thrCache:mutable.Map[(Int,Double),Double] = mutable.Map.empty
  // Test nonce cache for adversary
  var testCache:mutable.Map[(Int,Int),Double] = mutable.Map.empty

  /**
   * Blocks contain only information needed to construct tines
   * @param id unique block identifier (uniform random number)
   * @param pid parent block identifier (parent uniform random number)
   * @param n block number (label)
   * @param sl slot (label)
   * @param psl parent slot (parent label)
   * @param nonce eligibility (label)
   * @param adv adversarial block (boolean, default to false)
   */
  case class Block(
                    id:Int,
                    pid:Int,
                    n:Int,
                    sl:Int,
                    psl:Int,
                    nonce:Double,
                    adv:Boolean = false
                  )

  /**
   * Tines are a set of block ids with a head and a prefix
   * @param tineId
   * @param blockIds
   * @param prefix
   */
  case class Tine(
                   var tineId: Int, //id of the head of this tine
                   blockIds:mutable.Set[Int], //block identifiers comprising this tine
                   prefix:Int //id of the parent block at the beginning of this tine
                 ) {
    def extend(block:Block):Unit = {
      val headBlock = blockDb(tineId)
      //check for consistent labelling
      assert(block.pid == tineId)
      assert(block.sl > headBlock.sl)
      assert(block.n == headBlock.n+1)
      blockIds.add(block.id)
      tineId = block.id
    }
  }

  def reserve(tine:Tine,globalSlot:Int):Int = ???
  def gap(maxTine:Tine,tine:Tine):Int = ???
  def reach(maxTine:Tine,globalSlot:Int):Int = ???
  def margin(maxTine:Tine,tine:Tine,globalSlot:Int):Int = ???

  //challengers
  case class Honest(var head:Int, id:Int, relativeStake:Double) {
    //default chain selection rule
    def chainSelect(b:Block):Unit = {
      val cloc = blockDb(head)
      if (b.n > cloc.n) head = b.id //longest chain
    }

    //taktikos chain selection rule
    def chainSelect_tk(b:Block):Unit = {
      val cloc = blockDb(head)
      if (b.n > cloc.n) head = b.id //longest chain
      if (b.n == cloc.n && b.sl < cloc.sl) head = b.id
    }

    //arbitrary tie breaking rule, simply prefers lower block ids acting as a stand in for a VRF
    def chainSelect_tb(b:Block):Unit = {
      val cloc = blockDb(head)
      if (b.n > cloc.n) head = b.id
      if (b.n == cloc.n && b.id < cloc.id) head = b.id
    }

    //honest vrf test procedure
    def test(sl:Int):Option[Block] = {
      val pb = blockDb(head)
      val y = y_test(sl,id)
      val thr = phi(sl-pb.sl,relativeStake)
      if (y < thr) {
        Some(Block(
          rnd.nextInt(),
          pb.id,
          pb.n+1,
          sl,
          pb.sl,
          y
        ))
      } else {
        None
      }
    }
  }

  //players
  case class Adversarial(id:Int, relativeStake:Double) {
    /**
     * Defualt test strategy on the specified block identifier
     * @param sl slot to test
     * @param head parent block identifier
     * @return optional new block
     */
    def test(sl:Int,head:Int):Option[Block] = {
      val pb = blockDb(head)
      val delta = sl-pb.sl
      //cache test values
      val y = testCache.get((sl,id)) match {
        case Some(value) => value
        case None =>
          val newValue = y_test(sl,id)
          testCache.update((sl,id),newValue)
          newValue
      }
      //cache threshold values
      val thr = thrCache.get((delta,relativeStake)) match {
        case Some(value) => value
        case None =>
          val newValue = phi(delta,relativeStake)
          thrCache.update((delta,relativeStake),newValue)
          newValue
      }
      //test
      if (y < thr) {
        Some(Block(
          rnd.nextInt(),
          pb.id,
          pb.n+1,
          sl,
          pb.sl,
          y,
          adv = true
        ))
      } else {
        None
      }
    }

    /**
     * Adversarial testing strategy to produce branching tines
     * @param t time step to test up-to and including
     * @param b parent block
     * @return optional list of any blocks produced between parent slot and time step, all with same parent
     */
    def test_forward(t:Int,b:Block): List[Option[Block]] = {
      var out:List[Option[Block]] = List.empty
      for (i <- b.sl+1 to t) {
        test(i,b.id) match {
          case Some(block) => out ::= Some(block)
          case None =>
        }
      }
      out
    }
  }

  /**
   * Makes a copy of a block with the same labels and parent but a different unique identifier
   */
  def copyBlock_changeId(b:Block):Block = {
    Block(
      rnd.nextInt(), //make an arbitrary new fork
      b.pid,
      b.n,
      b.sl,
      b.psl,
      b.nonce,
      adv = true
    )
  }

  //threshold phi(delta,alpha)
  def phi(d:Int,a:Double): Double = {
    1.0 - math.pow(1-f(d),a)
  }

  //difficulty curve
  def f(d:Int):Double = {
    d match {
      case _ if d > gamma => f_B
      case _ if d < psi => 0.0
      case _ => f_A*(d-psi)/(gamma-psi).toDouble
    }
  }

  //some uniform randomness
  def sha256(bytes: Array[Byte]):Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(bytes)
    digest.digest()
  }

  //testing is assumed to follow VRF ideal functionality
  def y_test(sl:Int,id:Int):Double = {
    val testBytes = 2
    val uniqueBytes = sha256(Ints.toByteArray(sl)++Ints.toByteArray(id)++seed).take(testBytes)
    var net:Ratio = Ratio(0)
    var i:Int = 0
    for (byte <- uniqueBytes){
      i += 1
      val n = BigInt(byte & 0xff)
      val d = BigInt(2).pow(8*i)
      net = net + new Ratio(n,d)
    }
    net.toDouble
  }

  def updateDatabase(b:Option[Block]):Block = {
    blockDb.update(b.get.id,b.get)
    b.get
  }

  println(
    "*************************************************************************************"+
      "\n Settlement Game Simulation"+
      "\n*************************************************************************************"
  )

  //initial stake distribution split into uniform 'coins' across all automata
  val stakeDist:mutable.Map[Int,Double] = {
    val out:mutable.Map[Int,Double] = mutable.Map.empty
    Array.range(0,numAdversary).foreach(i=>out.update(i,alpha/numAdversary))
    Array.range(numAdversary,numHolders).foreach(i=>out.update(i,(1.0-alpha)/numHonest))
    out
  }

  val players: Array[Adversarial] = Array.range(0,numAdversary).map(p => Adversarial(p,stakeDist(p)))
  val challengers: Array[Honest] = Array.range(numAdversary,numHolders).map(p => Honest(0,p,stakeDist(p)))

  uniqueSlots.update(0,Set(0))

  // ids that the adversary may respond to, key is the honest block id, value is the adversarial id
  var blockResponses:mutable.Map[Int,Int] = mutable.Map.empty
  // distribution of settlements
  var settlements:List[Int] = List()
  // settlement counter, resets when adaptive adversary restarts balanced fork attack
  var k:Int = 1
  // slot that represents the beginning of the balanced fork that the adaptive adversary is constructing
  var s:Int = 0
  //empty string to be populated as sim executes, label space is {h,H,A}
  var wt = ""

  /**
   * Main simulation loop
   * Player is attempting to diverge the node-view of the challengers as much as possible
   */
  for (t <- 1 to T) {

    /**
     * Honest activity is represented here at the beginning of each round
     * Challengers chain select and deliver blocks to one another in order with a forced delay in slots
     * Every Delta-divergence is forced among challengers and the player may deliver blocks to the challenger any time
     */
    for ((id,delay) <- Random.shuffle(deliverySchedule.toList)) {
      if (delay > 0) {
        deliverySchedule.update(id,delay-1) //decrement delay
      } else {
        deliverySchedule.remove(id)
        blockResponses.get(id) match {
          case None =>
            challengers.foreach(h => h.chainSelect(blockDb(id))) //deliver blocks
          case Some(rid) =>
            //segment the network in 2, either due to honest tie, or player delivery of block response
            assert(blockDb(id).n == blockDb(rid).n)
            challengers.take(challengers.length/2).foreach(h => h.chainSelect(blockDb(id)))
            challengers.takeRight(challengers.length/2).foreach(h => h.chainSelect(blockDb(rid)))
        }
      }
    }

    val challengerHeads:Set[Int] = challengers.map(h => h.head).toSet

    if (challengerHeads.size == 1) {
      //only one node view among challengers, corresponds to unique convergence and settlement game should be reset!
      uniqueSlots.update(t,challengerHeads)
    } else {
      //more than one node view among challengers, settlement game keeps going...
      forkedSlots.update(t,challengerHeads)
    }

    //challengers make blocks with honest test strategy
    val challengerBlocks = challengers.map(h => h.test(t)).filter(_.isDefined).map(updateDatabase).toList

    //challengers diffuse blocks with a fixed delay
    for (block <- challengerBlocks) {
      deliverySchedule.update(block.id,bounded_delay)
    }

    //player reacts to new blocks
    challengerBlocks.nonEmpty match {

      /**
       * Empty trials, perpendicular symbol
       */
      case false => //player does nothing

      /**
       * Unique honest trials, h symbol
       */
      case true if challengerBlocks.size == 1 => // unique block

        //player will construct a balanced fork

      /**
       * Honest ties, H symbol:
       */
      case true if challengerBlocks.size > 1 => // honest tie

        //player will make an extension that breaks the tie

    }
  }
}

object SettlementGameSim {
  def main(args: Array[String]): Unit = {
    new SettlementGameSim
  }
}
