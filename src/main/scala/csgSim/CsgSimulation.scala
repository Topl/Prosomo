package csgSim

import com.google.common.primitives.Ints
import scala.collection.mutable
import scala.util.Random
import java.security.MessageDigest
import scala.util.control.Breaks.{breakable,break}

class CsgSimulation {

  //blocks only contain enough information to construct tines
  case class Block(sl:Int,psl:Int,n:Int,id:Int,pid:Int,adv:Boolean = false)

  trait Holder {
    val id:Int
    val alpha:Double
  }

  case class Honest(var head:Int, override val id:Int, override val alpha: Double) extends Holder {
    def chainSelect(b:Block):Unit = {
        val cloc = blockDb(head)
        if (b.n > cloc.n) head = b.id
        if (b.n == cloc.n && b.sl < cloc.sl) head = b.id
    }
    def test(sl:Int):Option[Block] = {
      val pb = blockDb(head)
      val y = y_test(sl,id)
      val thr = phi(sl-pb.sl,alpha)
      if (y < thr) {
        Some(Block(sl,pb.sl,pb.n+1,rnd.nextInt(),pb.id))
      } else {
        None
      }
    }
  }

  case class Adversarial(override val id:Int, override val alpha: Double) extends Holder {
    def test(sl:Int,head:Int):Option[Block] = {
      val pb = blockDb(head)
      val y = y_test(sl,id)
      val thr = phi(sl-pb.sl,alpha)
      if (y < thr) {
        Some(Block(sl,pb.sl,pb.n+1,rnd.nextInt(),pb.id,adv = true))
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

  println("***************************************************************************************************"+
  "\n Conditional Settlement Game Simulation:"+
  "\n Challegers are honestly behaving automata"+
  "\n Adversaries are automata attempting to execute a balanced fork attack by forging nothing-at-stake"+
  "\n***************************************************************************************************")

  val printGame = true
  val numHolders = 100
  val numAdversary = 40
  val numHonest:Int = numHolders - numAdversary
  val T = 10000
  val rnd:Random = new Random
  val seed:Array[Byte] = Array.fill(32){0x00.toByte}
  val forkedSlots:mutable.Map[Int,Seq[Block]] = mutable.Map.empty
  val uniqueSlots:mutable.Map[Int,Seq[Block]] = mutable.Map.empty
  rnd.nextBytes(seed)

  val gamma = 40
  val psi = 0
  val f_A = 0.4
  val f_B = 0.1

  val stakeDist:mutable.Map[Int,Double]= {
    val out:mutable.Map[Int,Double] = mutable.Map.empty
    Array.range(0,numHolders).foreach(i=>out.update(i,1.0/numHolders))
    out
  }

  val blockDb:mutable.Map[Int,Block] = mutable.Map(0 -> Block(0,0,0,0,0))
  var numTines = 1
  val adversaryWindow = 100
  val challengers: Array[Honest] = Array.range(0,numHonest).map(p => Honest(0,p,stakeDist(p)))
  val adversaries: Array[Adversarial] = Array.range(0,numAdversary).map(p => Adversarial(-p-1,stakeDist(p)))

  uniqueSlots.update(0,Seq(blockDb(0)))
  def update(b:Option[Block]):Block = {
    blockDb.update(b.get.id,b.get)
    b.get
  }

  var settlements:List[Int] = List()
  var settlementCounter:Int = 0

  var windowBlocks:List[Block] = List(blockDb(0))
  def filterWindow(sl:Int):Unit =
    windowBlocks = windowBlocks.filter(b => b.sl > sl - adversaryWindow)

  for (i <- 1 to T) {

    val honestBlocks = Random.shuffle(challengers.map(h => h.test(i)).filter(_.isDefined).map(update).toList).take(2)

    val commonId:Int = challengers.map(h => blockDb(h.head).id).groupBy(i => i).mapValues(_.length).maxBy(_._2)._1

    val staticAdversaryBlocks = adversaries.map(a => a.test(i,commonId)).filter(_.isDefined) match {
      case blocks if blocks.length >= 2 => blocks.take(2).map(update).toList
      case blocks if blocks.length == 1 =>
        val b = blocks.head.get
        List(Some(b),Some(Block(b.sl,b.psl,b.n,rnd.nextInt(),b.pid,adv = true))).map(update)
      case _ =>
        List()
    }

    (honestBlocks.nonEmpty,staticAdversaryBlocks.nonEmpty) match {
      case (false,false) =>
      case (true,false) if honestBlocks.size == 1 =>
        numTines match { case 2 => numTines -= 1 case _ => }
        if (printGame) println("|   0")
        settlementCounter += 1
        settlements ::= settlementCounter
        settlementCounter = 0
        uniqueSlots.update(i,honestBlocks)
        Random.shuffle(challengers.toList).foreach(h => {
          h.chainSelect(honestBlocks.head)
        })
      case _ =>
        numTines match {
          case 2 => if (printGame) println("| | 1")
          case 1 =>
            numTines += 1
            if (printGame) println("|\\  1")
        }
        settlementCounter += 1
        forkedSlots.update(i,honestBlocks++staticAdversaryBlocks)
        Random.shuffle(challengers.toList).foreach(h => {
          Random.shuffle(honestBlocks++staticAdversaryBlocks).foreach(b => {
            h.chainSelect(b)
          })
        })
    }
  }

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
  println(s"Proportion of adversarial stake: ${adversaries.map(_.alpha).sum}")
  println(s"Proportion of unique slots p_0/(p_0+p_1) = ${uniqueSlots.keySet.size.toDouble/(uniqueSlots.keySet.size+forkedSlots.keySet.size)}")
  println(s"Expectation of settlement depth in blocks: ${settlements.sum.toDouble/settlements.size}")
}

object CsgSimulation extends App {
  new CsgSimulation
}
