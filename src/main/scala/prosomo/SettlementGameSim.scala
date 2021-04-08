package prosomo

import java.security.MessageDigest

import com.cibo.evilplot.numeric.Point
import com.google.common.primitives.Ints
import plotly.element.ScatterMode
import plotly.layout._
import plotly.{Plotly, Scatter}
import prosomo.primitives.Ratio

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
  val T = 5000
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

  var fork: mutable.Map[Int, Tine] = mutable.Map.empty
  val initTine: Tine = Tine(rnd.nextInt(),mutable.Set.empty, mutable.Map.empty)
  fork += (initTine.tineId -> initTine)


  val numForwardSlots = 100
  val depth = 4

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
   * @param prefixes
   */
  case class Tine(
                   var tineId: Int, //id of the head of this tine
                   blockIds:mutable.Set[Int], //block identifiers comprising this tine
                   prefixes:mutable.Map[Int,Int] //id of the parent block at the beginning of this tine
                 ) {
    /*def extend(block:Block):Unit = {
      val headBlock = blockDb(tineId)
      //check for consistent labelling
      //assert(block.pid == tineId)
      assert(block.sl > headBlock.sl)
      assert(block.n == headBlock.n+1)
      blockIds.add(block.id)
      //tineId = block.id
    }*/
  }

  // This function returns ID of a longest tine
  def getLongestTine(): Tine = {
    var maxLength = 0
    var maxTineId = 0
    val Z: mutable.Set[Int] = mutable.Set.empty
    val R: mutable.Set[Int] = mutable.Set.empty

    for(tine <- fork){

      if(tine._2.blockIds.size>= maxLength ){
        maxLength = tine._2.blockIds.size
        maxTineId = tine._1
      }
    }
    fork(maxTineId)
  }
  def reserve(tine:Tine,numSlot: Int):Int = {

    val headTine = blockDb(tine.blockIds.head)

    val listBlocks = players.map(h => h.test_forward(numSlot,tine)).toList

    listBlocks(0).size
  }
  def gap(maxTine:Tine,tine:Tine):Int = {
    val gap = maxTine.blockIds.size - tine.blockIds.size
    gap
  }
  def reach(tine:Tine):Int = {
    val longestTine = getLongestTine()
    val gapValue = gap(longestTine,tine)
    reserve(tine,numForwardSlots) - gapValue
  }
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


    def testFromAnyBlock(sl:Int,pb:Block):Option[Block] = {
      //val pb = blockDb(head)
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



    def MaxLengthForwardTine(forwardFork :  mutable.Map[Int,Tine]) : Tine = {

      var maxTineTemp: Tine = Tine(rnd.nextInt(),mutable.Set.empty, mutable.Map.empty)
      for(tine <- forwardFork){
        if(tine._2.blockIds.size >= maxTineTemp.blockIds.size){
          maxTineTemp = tine._2
        }
      }
      maxTineTemp
    }
    /**
     * Adversarial testing strategy to produce branching tines
     * @param t time step to test up-to and including
     * @param initB parent block
     * @return optional list of any blocks produced between parent slot and time step, all with same parent
     */
    /*def test_forward(t:Int,b:Block): List[Option[Block]] = {
      var out:List[Option[Block]] = List.empty
      for (i <- b.sl+1 to t) {
        test(i,b.id) match {
          case Some(block) => out ::= Some(block)
          case None =>
        }
      }
      out
    }*/
    def test_forward(t:Int,tineInput: Tine): List[Option[Block]] = {
      var out:List[Option[Block]] = List.empty
      var pb: Block = Block(0, 0, 0, t, 0, 0)
      if(!tineInput.blockIds.isEmpty){
        pb = blockDb(tineInput.blockIds.head)
      }


      // Idea here is that we set the initial parent block (head of the tine) as a genesis block, and create all possible tines.
      var forwardFork: mutable.Map[Int, Tine] = mutable.Map.empty

      val forwardBlockDb:mutable.Map[Int,Block] = mutable.Map(pb.id -> pb)
      val tine: Tine = Tine(rnd.nextInt(),mutable.Set(pb.id), mutable.Map.empty)
      forwardFork = mutable.Map(tine.tineId -> tine)
      println("Parent Block : "+pb.id)

      var x: mutable.IndexedSeq[Double] = mutable.IndexedSeq.empty
      var y: mutable.IndexedSeq[Double] = mutable.IndexedSeq.empty

      for (i <- pb.sl+1 to pb.sl+t) {
        //val maxTineForward = MaxLengthForwardTine(forwardFork)
        for(tempTine <- forwardFork){
         // if(maxTineForward.blockIds.size - t._2.blockIds.size <= depth){
          println("Block Head: "+tempTine._2.blockIds.head)
            testFromAnyBlock(i,forwardBlockDb(tempTine._2.blockIds.head)) match {
              case Some(block) => {
                forwardBlockDb += (block.id -> block)
                println(i - pb.sl+" Forward Block : "+block.id + "Tine ID: "+tempTine._1)
                val newTine = copyTine_changeId(tempTine._2,block)
                forwardFork += (newTine.tineId -> newTine)
                x +:= block.sl.toDouble
                y+:= block.nonce.toDouble
              }
              case None =>
            }
          //}


        }

      }

      /*var x_forward: mutable.Map[Int,mutable.IndexedSeq[Double]] = mutable.Map.empty
      var y_forward: mutable.Map[Int,mutable.IndexedSeq[Double]] = mutable.Map.empty



      println("Number of Tine: "+forwardFork.size)

      for(tine <- forwardFork) {
        for (id <- tine._2.blockIds) {
          //print(forwardBlockDb(id).sl.toDouble)
          if(!x_forward.isDefinedAt(tine._1)){
            x_forward += (tine._1 -> mutable.IndexedSeq(forwardBlockDb(id).sl.toDouble))
          }else{
            x_forward(tine._1) :+= forwardBlockDb(id).sl.toDouble
          }

          if(!y_forward.isDefinedAt(tine._1)){
            y_forward += (tine._1 -> mutable.IndexedSeq(forwardBlockDb(id).nonce.toDouble))
          }else{
            y_forward(tine._1) :+= forwardBlockDb(id).nonce.toDouble
          }



        }
        if(!x_forward(tine._1).isEmpty){
          val trace_forward = Scatter(
            x_forward(tine._1),
            y_forward(tine._1),
            mode = ScatterMode(ScatterMode.Markers)
          )
          println("TEST FORWARD "+ x_forward(tine._1))
          val layout = Layout(
            title = "Test Forward"
          )
          /*val traceMaxforward = Scatter(
            x_forward(outTine.tineId),
            y_forward(outTine.tineId),
            mode = ScatterMode(ScatterMode.Markers)
          )*/
          val data_forward = Seq(trace_forward)

          //Plotly.plot("plot_forward"+pb.sl+".html", data_forward, layout)
        }

      }*/

      val trace = Scatter(
        x,
        y,
        mode = ScatterMode(ScatterMode.Markers)
      )
      val outTine = MaxLengthForwardTine(forwardFork)

      var xMax: mutable.IndexedSeq[Double] = mutable.IndexedSeq.empty
      var yMax: mutable.IndexedSeq[Double] = mutable.IndexedSeq.empty
      for(b <- outTine.blockIds){
        out ::= Some(forwardBlockDb(b))
        xMax +:= forwardBlockDb(b).sl.toDouble
        yMax +:= forwardBlockDb(b).nonce.toDouble
      }

      val traceMax = Scatter(
        xMax,
        yMax,
        mode = ScatterMode(ScatterMode.Markers)
      )

      val layout = Layout(
        title = "Test Forward at Slot "+pb.sl
      )

      val data_forward = Seq(trace,traceMax)

      Plotly.plot("plot_forward"+pb.sl+".html", data_forward, layout)
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


  def copyTine_changeId(tine: Tine,block: Block): Tine = {

    //copy blockIDs
    val newBlockIds: mutable.Set[Int] = mutable.Set.empty
    val newPrefixes: mutable.Map[Int, Int] = mutable.Map.empty

    for(id <- tine.blockIds){
      newBlockIds.add(id)
    }

    for(prefix <- tine.prefixes){
      newPrefixes += (prefix._1 -> prefix._2)
    }

    newBlockIds.add((block.id))
    val newTine: Tine = Tine(rnd.nextInt(),newBlockIds,newPrefixes)
    newTine
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
    /*for ((id,delay) <- Random.shuffle(deliverySchedule.toList)) {
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
    }*/

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
    /*for (block <- challengerBlocks) {
      deliverySchedule.update(block.id,bounded_delay)
    }*/

    val maxTine = getLongestTine()
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

        maxTine.blockIds.add(challengerBlocks.head.id)
        //println("Tine length "+maxTine.blockIds.size)

      /**
       * Honest ties, H symbol:
       */
      case true if challengerBlocks.size > 1 => // honest tie
        println("Round "+t)
        //player will make an extension that breaks the tie
        var maxReach = 0
        var maxReachId = 0
        for(tine <- fork){
          val tempReach = reach(tine._2)
          if(maxReach <= reach(tine._2)){
            maxReachId = tine._1
            maxReach = tempReach
          }
        }


        maxTine.blockIds.add(challengerBlocks.toList(0).id)

        if(maxReachId == maxTine.tineId){

          val newTine = copyTine_changeId(maxTine,challengerBlocks.toList(1))
          fork += (newTine.tineId -> newTine)
        }
        else{
          fork(maxReachId).blockIds.add(challengerBlocks.toList(1).id)
        }
        println("Fork Size "+fork.size)


    }
  }
  val data = Seq.tabulate(100) { i =>
    Point(i.toDouble, scala.util.Random.nextDouble())
  }
  var x: mutable.IndexedSeq[Double] = mutable.IndexedSeq.empty
  var y: mutable.IndexedSeq[Double] = mutable.IndexedSeq.empty
  val y1 = x.map(d => 2.0 * d + util.Random.nextGaussian())
  val y2 = x.map(math.exp)




  for(tine <- fork){
    println("Tine: "+tine._2.blockIds)
    for(id <- tine._2.blockIds){
      print(blockDb(id).sl.toDouble)
      x :+= blockDb(id).sl.toDouble
      y :+= blockDb(id).nonce.toDouble
      println("X"+x.toList.length)

     //data += (Point(blockDb(id).sl.toDouble, blockDb(id).nonce.toDouble))

    }
    val trace1 = Scatter(
      x,
      y,
      mode = ScatterMode(ScatterMode.Markers)
    )
    val layout = Layout(
      title = "Taktikos Simulation"
    )

    val data = Seq(trace1)
    Plotly.plot("plot.html", data, layout)


    println("X"+x)
    println("Y"+y)
  }


}

object SettlementGameSim {
  def main(args: Array[String]): Unit = {
    new SettlementGameSim
  }
}
