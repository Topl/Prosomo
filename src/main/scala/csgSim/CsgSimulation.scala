package csgSim

import com.google.common.primitives.Ints

import scala.collection.mutable
import scala.util.Random
import java.security.MessageDigest
import scala.util.control.Breaks.{breakable,break}

class CsgSimulation {

  println("*************************************************************************************"+
    "\n Conditional Settlement Game Simulation:"+
    "\n Challegers are honest players"+
    "\n Adversaries are Nothing-at-Stake attackers trying to execute a balanced fork attack"+
    "\n*************************************************************************************")

  val numHolders = 1000
  val numAdversary = 400
  val numHonest:Int = numHolders - numAdversary
  val T = 10000
  val rnd:Random = new Random
  val seed:Array[Byte] = Array.fill(32){0x00.toByte}
  val forkedSlots:mutable.Map[Int,Seq[Block]] = mutable.Map.empty
  val uniqueSlots:mutable.Map[Int,Seq[Block]] = mutable.Map.empty

  rnd.nextBytes(seed)

  val gamma = 40
  val psi = 5

  val f_A = 0.4
  val f_B = 0.1

  def Sha256(bytes: Array[Byte]):Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(bytes)
    digest.digest()
  }

  //testing is assumed to follow VRF ideal functionality
  def y_test(sl:Int,id:Int):Double = {
    val uniqueBytes = Sha256(Ints.toByteArray(sl)++Ints.toByteArray(id)++seed).take(2)
    (BigInt(uniqueBytes)+32768).toDouble/65535
  }

  //difficulty curve
  def f(d:Int):Double = {
    d match {
      case _ if d > gamma => f_B
      case _ if d < psi => 0.0
      case _ => f_A*(d-psi)/(gamma-psi).toDouble
    }
  }

  //threshold phi(delta,alpha)
  def phi(d:Int,a:Double): Double = {
    1.0 - math.pow(1-f(d),a)
  }

  //blocks only contain enough information to construct tines
  case class Block(sl:Int,psl:Int,n:Int,id:Int,pid:Int,adv:Boolean = false)

  val blockDb:mutable.Map[Int,Block] = mutable.Map(0 -> Block(0,0,0,0,0))
  uniqueSlots.update(0,Seq(blockDb(0)))

  val stakeDist:mutable.Map[Int,Double]= {
    val out:mutable.Map[Int,Double] = mutable.Map.empty
    Array.range(0,numHolders).foreach(i=>out.update(i,1.0/numHolders))
    out
  }

  trait Holder {
    val id:Int
    val alpha:Double
  }

  case class Honest(var head:Int, override val id:Int, override val alpha: Double) extends Holder {
    def chainSelect(b:Option[Block]):Unit = b match {
      case Some(block) =>
        val cloc = blockDb(head)
        if (block.n > cloc.n) head = block.id
        if (block.n == cloc.n && block.sl < cloc.sl) head = block.id
      case _ =>
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

  val challengers: Array[Honest] = Array.range(0,numHonest).map(p => Honest(0,p,stakeDist(p)))
  val adversaries: Array[Adversarial] = Array.range(0,numAdversary).map(p => Adversarial(-p-1,stakeDist(p)))


  var windowBlocks:List[Block] = List(blockDb(0))
  var numTines = 1
  for (i <- 1 to T) {
    //println("Slot: "+i.toString)
    var roundBlocks:List[Block] = List()
    windowBlocks = windowBlocks.filter(b => b.sl > i - 100)
    challengers.foreach(h => h.test(i) match {
      case Some(b) => roundBlocks ++= List(b)
      case None =>
    })
    breakable{
      adversaries.foreach(a =>
        windowBlocks.foreach(b => a.test(i,b.id) match {
          case Some(block) if roundBlocks.isEmpty => roundBlocks ++= List(block,a.test(i,b.id).get);
          case Some(block) if roundBlocks.size == 1 => roundBlocks ++= List(block);
          case Some(block) if roundBlocks.size > 1 =>
          case None =>
        })
      )
    }

    roundBlocks.foreach(b => blockDb.update(b.id,b))

    if (roundBlocks.nonEmpty) {
      roundBlocks.size match {
        case 1 if numTines == 1 => println("|"); uniqueSlots.update(i,roundBlocks)
        case 2 if numTines == 1 => println("|\\"); numTines += 1; forkedSlots.update(i,roundBlocks)
        case 1 if numTines == 2 => println("|"); numTines -= 1; uniqueSlots.update(i,roundBlocks)
        case 2 if numTines == 2 => println("| |"); forkedSlots.update(i,roundBlocks)
        case _ => println("| |"); forkedSlots.update(i,roundBlocks)
      }
      Random.shuffle(challengers.toList).foreach(h =>
        h.chainSelect(Some(Random.shuffle(roundBlocks).head))
      )
      windowBlocks ++= roundBlocks
    }
  }
  val headMode = challengers.map(h => blockDb(h.head).id).groupBy(i => i).mapValues(_.length).maxBy(_._2)._1
  val maxBn = blockDb(headMode).n
  println("Local chain head id and block number: "+headMode.toString+", "+maxBn.toString)
  var numAdvBlocks = 0
  breakable {
    var head = headMode
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
  val ratioAdvHonest:Double = numAdvBlocks.toDouble/maxBn
  println("Proportion of adversarial blocks: "+ratioAdvHonest.toString)



}

object CsgSimulation extends App {
  new CsgSimulation
}