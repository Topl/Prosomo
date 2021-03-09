package csgSim

import com.google.common.primitives.Ints
import scala.collection.mutable
import scala.util.Random
import java.security.MessageDigest
import scala.util.control.Breaks.{breakable,break}

/**
 * AMS 2021: Conditional Settlement Game Simulator
 */

class CsgSimulation {

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

  //abstract trait for automata
  trait Holder {
    val id:Int
    val alpha:Double
  }

  //challengers
  case class Honest(var head:Int, override val id:Int, override val alpha: Double) extends Holder {
    def chainSelect(b:Block):Unit = {
        val cloc = blockDb(head)
        if (b.n > cloc.n) head = b.id //longest chain
        if (b.n == cloc.n && b.sl < cloc.sl) head = b.id //tk 'early bird' rule
    }
    def test(sl:Int):Option[Block] = {
      val pb = blockDb(head)
      val y = y_test(sl,id)
      val thr = phi(sl-pb.sl,alpha)
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
  case class Adversarial(override val id:Int, override val alpha: Double) extends Holder {
    def test(sl:Int,head:Int):Option[Block] = {
      val pb = blockDb(head)
      val y = y_test(sl,id)
      val thr = phi(sl-pb.sl,alpha)
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
    val uniqueBytes = sha256(Ints.toByteArray(sl)++Ints.toByteArray(id)++seed).take(2)
    (BigInt(uniqueBytes)+32768).toDouble/65535
  }

  println(
    "*************************************************************************************"+
  "\n Conditional Settlement Game Simulation:"+
  "\n Challengers are honestly behaving automata"+
  "\n Players are adversaries, i.e. automata attempting to execute a balanced fork attack"+
  "\n*************************************************************************************"
  )

  //print the 1's and 0's
  val printGame = true
  //total number of automata
  val numHolders = 100
  //total number of players
  val numAdversary = 40
  //total number of challengers
  val numHonest:Int = numHolders - numAdversary
  //number of rounds to be executed
  val T = 10000
  //generates random IDs for blocks
  val rnd:Random = new Random
  //epoch nonce in VRF tests
  val seed:Array[Byte] = Array.fill(32){0x00.toByte}
  rnd.nextBytes(seed)
  //map of slots that have more than one block
  val forkedSlots:mutable.Map[Int,Seq[Block]] = mutable.Map.empty
  //map of slots that have one block
  val uniqueSlots:mutable.Map[Int,Seq[Block]] = mutable.Map.empty
  //forging window cutoff
  val gamma = 40
  //slot gap
  val psi = 0
  //max difficulty
  val f_A = 0.4
  //base difficulty
  val f_B = 0.1
  //initial stake distribution split into uniform 'coins' across all automata
  val stakeDist:mutable.Map[Int,Double] = {
    val out:mutable.Map[Int,Double] = mutable.Map.empty
    Array.range(0,numHolders).foreach(i=>out.update(i,1.0/numHolders))
    out
  }
  //database of all blocks, begins with genesis block, key is block id
  val blockDb:mutable.Map[Int,Block] = mutable.Map(0 -> Block(0,0,0,0,0,0))
  //counter for number of viable tines in the forging window
  var numTines = 1
  //window of slots in which the player attempts to extend tines
  val adversaryWindow = 100

  val challengers: Array[Honest] = Array.range(0,numHonest).map(p => Honest(0,p,stakeDist(p)))
  val players: Array[Adversarial] = Array.range(0,numAdversary).map(p => Adversarial(-p-1,stakeDist(p)))

  uniqueSlots.update(0,Seq(blockDb(0)))

  def update(b:Option[Block]):Block = {
    blockDb.update(b.get.id,b.get)
    b.get
  }

  //distribution of settlements
  var settlements:List[Int] = List()
  //settlement counter
  var k:Int = 1

  var windowBlocks:List[Block] = List(blockDb(0))

  def filterWindow(sl:Int):Unit =
    windowBlocks = windowBlocks.filter(b => b.sl > sl - adversaryWindow)

  //player will forge this number of blocks in each round where eligible
  //set to 0 for default behavior
  val spam_blocks = 0

  //start the simulation
  for (i <- 1 to T) {

    //challengers make blocks
    val honestBlocks = Random.shuffle(challengers.map(h => h.test(i)).filter(_.isDefined).map(update).toList).take(2)

    //get the most common block id corresponding to the head of the chain among all challengers
    val commonId:Int = challengers.map(h => blockDb(h.head).id).groupBy(i => i).mapValues(_.length).maxBy(_._2)._1

    val staticAdversaryBlocks:List[Block] = players.map(a => a.test(i,commonId)).filter(_.isDefined) match {
      case blocks if blocks.length > 0 && spam_blocks > 0 =>
        val b = blocks.head.get
        Array.range(0,spam_blocks).toList.map(_ => Some(Block(
          rnd.nextInt(), //make an arbitrary new fork, while copying the labels of the block, making 2 blocks
          b.pid,
          b.n,
          b.sl,
          b.psl,
          b.nonce,
          adv = true
        ))).map(update)
      //static adversary creates two blocks with each leadership eligibility
      case blocks if blocks.length >= 2 => blocks.take(2).map(update).toList //player is only interested in 2 blocks
      case blocks if blocks.length == 1 =>
        val b = blocks.head.get
        List(
          Some(b),
          Some(Block(
            rnd.nextInt(), //make an arbitrary new fork, while copying the labels of the block, making 2 blocks
            b.pid,
            b.n,
            b.sl,
            b.psl,
            b.nonce,
            adv = true
          ))).map(update)
      case _ =>
        List()
    }
    //main game logic
    (honestBlocks.nonEmpty,staticAdversaryBlocks.nonEmpty) match {
      /**
       * Empty trials, player does nothing
       */
      case (false,false) => // perpendicular symbol, do nothing
      /**
       * Unique honest trials, player resets game
       */
      case (true,false) if honestBlocks.size == 1 => // H_0, unique block, player sets k = 1
        numTines match { case 2 => numTines -= 1 case _ => }
        if (printGame) println("|   0")
        settlements ::= k
        k = 1
        uniqueSlots.update(i,honestBlocks)
        Random.shuffle(challengers.toList).foreach(h => {
          h.chainSelect(honestBlocks.head)
        })

      /**
       * Adversarial and Honest ties, player constructs malicious tines to bias challengers and extend balanced fork
       * AMS 2021:  Optimal player strategy is to be determined, for now static player strategy is deployed
       */
      case _ => //H_1 and A_1 symbols, player may construct arbitrary forks
        numTines match {
          case 2 => if (printGame) println("| | 1")
          case 1 =>
            numTines += 1
            if (printGame) println("|\\  1")
        }
        k += 1
        forkedSlots.update(i,honestBlocks++staticAdversaryBlocks)
        Random.shuffle(challengers.toList).foreach(h => {
          Random.shuffle(honestBlocks++staticAdversaryBlocks).foreach(b => {
            h.chainSelect(b)
          })
        })
    }
  }

  //retrieves the mode of the head among challenger's local chain
  val commonId:Int = challengers.map(h => blockDb(h.head).id).groupBy(i => i).mapValues(_.length).maxBy(_._2)._1
  val maxBlockNumber:Int = blockDb(commonId).n
  println("Local chain head id and block number: "+commonId.toString+", "+maxBlockNumber.toString)
  var numAdvBlocks = 0
  breakable {
    var head = commonId
    while (true) {
      val block = blockDb(head)
      if (block.n > 0) {
        if (block.adv) {
          numAdvBlocks += 1
        }
        head = block.pid
      } else {
        break()
      }
    }
  }
  println(s"************** Static Adversary ******************")
  println(s"Proportion of adversarial blocks on dominant tine: ${numAdvBlocks.toDouble/maxBlockNumber}")
  println(s"Proportion of adversarial stake: ${players.map(_.alpha).sum}")
  println(s"Proportion of unique slots p_0/(p_0+p_1) = ${uniqueSlots.keySet.size.toDouble/(uniqueSlots.keySet.size+forkedSlots.keySet.size)}")
  println(s"Expectation of settlement depth in blocks: ${settlements.sum.toDouble/settlements.size}")
}

object CsgSimulation extends App {
  new CsgSimulation
}
