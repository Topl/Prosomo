package csgSim

import com.google.common.primitives.Ints
import prosomo.primitives.Ratio

import scala.collection.mutable
import scala.util.Random
import java.security.MessageDigest
import scala.util.control.Breaks.{break, breakable}
import scala.math.BigInt

/**
 * AMS 2021: Conditional Settlement Game Simulator
 * This represents a simple toy model of adversarial behavior in the execution of Ouroboros.
 * The adaptive adversary using local-dynamic-difficulty is modeled with a set of automata that behave according
 * to a longest chain selection rule.  Idealized representations of the leadership election mechanism and selection rule
 * are implemented with the intention of numerical efficiency.  This provides a performant test bed
 * to explore the nature of the adaptive adversary with a conditional leader election process.
 * Block ids are represented with randomly generated integers and the structure of tines can be constructed from the
 * labels (slots, nonces, and block number)
 */

class SettlementGameSimTaktikos{

  //print visuals as the simulation executes
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
  val f_B = 0.05
  //initial stake distribution split into uniform 'coins' across all automata
  val stakeDist:mutable.Map[Int,Double] = {
    val out:mutable.Map[Int,Double] = mutable.Map.empty
    Array.range(0,numHolders).foreach(i=>out.update(i,1.0/numHolders))
    out
  }
  //database of all blocks, begins with genesis block, key is block id
  val blockDb:mutable.Map[Int,Block] = mutable.Map(0 -> Block(0,0,0,0,0,0))
  //counter for number of viable tines in the forging window

  val challengers: Array[Honest] = Array.range(0,numHonest).map(p => Honest(0,p,stakeDist(p)))
  val players: Array[Adversarial] = Array.range(0,numAdversary).map(p => Adversarial(-p-1,stakeDist(p)))

  uniqueSlots.update(0,Seq(blockDb(0)))

  var fork: mutable.Map[Int, Prefixes] = mutable.Map.empty

  // distribution of settlements
  var settlements:List[Int] = List()
  // settlement counter, resets when adaptive adversary restarts balanced fork attack
  var k:Int = 1
  // slot that represents the beginning of the balanced fork that the adaptive adversary is constructing
  var s:Int = 0
  // block id of the x point of the balanced fork
  var xid:Int = 0
  // Delta, the total delay in slots in the bounded delay semi-synchronous setting
  val bounded_delay = 10
  // Tracks honest blocks for delivery after set delay of bounded_delay slots, delay decrements with time step
  // Key is block id, value is delay that decrements with each time step
  val deliverySchedule:mutable.Map[Int,Int] = mutable.Map.empty
  // Adversarial reserve
  var covertBlocks:List[Block] =  List.empty
  // Honest blocks in the current branch
  var honestBlocks:List[Block] = List.empty
  // Leading Unique Honest block
  var leadingBlock:Int = 0
  // Block ids that the adversary may respond to, key is the honest block id, value is the adversarial id
  var blockResponses:mutable.Map[Int,Int] = mutable.Map.empty
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

  case class Tine(
                   var TineId: Int,
                   var gap: Int,
                   var reserve: Int,
                   var reach: Int,
                   blocks: mutable.Map[Int,Block]
                 )

  case class Prefixes(
                       var tine: Tine,
                       var positions: mutable.Map[Int, Int]
                     )



  //challengers
  case class Honest(var head:Int, id:Int, alpha:Double) {

    def chainSelect(b:Block):Unit = {
      val cloc = blockDb(head)
      if (b.n > cloc.n) head = b.id //longest chain
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
  case class Adversarial(id:Int,alpha:Double) {

    def test(sl:Int,head:Int):Option[Block] = {
      val pb = blockDb(head)
      val delta = sl-pb.sl
      val y = testCache.get((sl,id)) match {
        case Some(value) => value
        case None =>
          val newValue = y_test(sl,id)
          testCache.update((sl,id),newValue)
          newValue
      }

      val thr = thrCache.get((delta,alpha)) match {
        case Some(value) => value
        case None =>
          val newValue = phi(delta,alpha)
          thrCache.update((delta,alpha),newValue)
          newValue
      }

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

  //makes a copy of a block with the same labels but a different unique identifier
  def copyBlock(b:Block):Block = {
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

  def getLongestTine(): Int = {
    var maxLength = 0
    var maxTineId = 0
    val Z: mutable.Set[Int] = mutable.Set.empty
    val R: mutable.Set[Int] = mutable.Set.empty

    for(tine <- fork){

      if(tine._2.tine.blocks.size >= maxLength ){
        maxLength = tine._2.tine.blocks.size
        maxTineId = tine._2.tine.TineId
      }
    }
    maxTineId
  }

  def getZeroReachandMaxReachSets(): (mutable.Set[Prefixes],mutable.Set[Prefixes]) = {

    var Z: mutable.Set[Prefixes] = mutable.Set.empty
    var Z_2: mutable.Map[Int,Prefixes] = mutable.Map.empty
    var R: mutable.Set[Prefixes] = mutable.Set.empty
    var MaxReach: Int = 0

    for(tine <- fork){

      if(tine._2.tine.reach >= MaxReach){
        MaxReach = tine._2.tine.reach
      }
      if(tine._2.tine.reach  == 0 ){
        Z_2 += (Z_2.size+1 -> tine._2)

      }
    }
    for(tine <- fork){

      if(tine._2.tine.reach  == MaxReach ){
        R += tine._2
      }
    }

    (Z,R)
  }

  def getZeroReachSet(): mutable.Set[Prefixes] = {

    var Z: mutable.Set[Prefixes] = mutable.Set.empty

    for(tine <- fork){
      if(tine._2.tine.reach  == 0 ){
        Z.add(tine._2)
      }
    }
    Z
  }

  def getMaxReachSet(): mutable.Set[Prefixes] = {


    var R: mutable.Set[Prefixes] = mutable.Set.empty
    var MaxReach: Int = 0

    for(tine <- fork){

      if(tine._2.tine.reach >= MaxReach){
        MaxReach = tine._2.tine.reach
      }

    }
    for(tine <- fork){

      if(tine._2.tine.reach  == MaxReach ){
        R.add(tine._2)
      }
    }

    R
  }
  def updateGap() {
    val bestTine = getLongestTine()
    var maxTine = 0
    for(tine <- fork){
      if(tine._2.tine.TineId == bestTine){
        maxTine = tine._2.tine.blocks.size
      }
    }

    for(tine <- fork){
      tine._2.tine.gap = maxTine - tine._2.tine.blocks.size
    }

  }

  def updateReserve(currSlot: Int, charString: String){
    var reserve = 0
    for(i <- currSlot until charString.length){
      if(charString(i).equals("A")){
        reserve += 1
      }
    }

    // update reserve and reach
    for(tine <- fork){
      tine._2.tine.reserve = reserve
      tine._2.tine.reach= reserve - tine._2.tine.gap
    }
  }



  def copyBlocks(blocks: mutable.Map[Int,Block]): mutable.Map[Int,Block] = {
    var newMapBlocks: mutable.Map[Int,Block] = mutable.Map.empty
    for(block <- blocks){
      val key = block._1
      val value = block._2
      newMapBlocks += (key -> value)
    }
    newMapBlocks
  }


  def copyPositions(positions: mutable.Map[Int, Int]): mutable.Map[Int, Int] = {
    var newMapPos: mutable.Map[Int, Int] = mutable.Map.empty
    for (position <- positions) {
      val key = position._1
      val value = position._2
      newMapPos += (key -> value)
    }
    newMapPos
  }

  def createFork(w: String) ={

    for (t <- 0 until w.length-1){

      if(w.toList(t).equals('h')) {
        val bestTine = getLongestTine()
        val newBlock: Block = Block(rnd.nextInt(), t, t, t, t, rnd.nextInt())

        for(tine <- fork){
          if(tine._2.tine.TineId == bestTine){
            tine._2.tine.blocks += (tine._2.tine.blocks.size+1 -> newBlock)
            //println("Length of longest tine: "+tine._2.tine.blocks.size)
          }
        }
        updateGap()
        updateReserve(t,w)
      }
      else if (w(t).equals('H')){
        val Z: mutable.Set[Prefixes] = getZeroReachSet()
        val R: mutable.Set[Prefixes] = getMaxReachSet()

        //val (Z,R) = getZeroReachandMaxReachSets()
        // find r_1 and z_1
        var min = 99999999
        val newBlock: Block = Block(rnd.nextInt(), t, t, t, t, rnd.nextInt())
        val temp:mutable.Map[Int,Block] =  mutable.Map(t -> newBlock)
        val tine: Tine = Tine(rnd.nextInt(),0,0,0,temp)
        val tineWithPrefixes = Prefixes(tine, mutable.Map.empty)
        var r_1: Prefixes = Prefixes(tine, mutable.Map.empty)
        var z_1: Prefixes = Prefixes(tine, mutable.Map.empty)

        for(r <- R){
          for(z <- Z){

            // prefer having two different tines with two different head
            if(r.tine.TineId != z.tine.TineId){
              if(r.positions.getOrElse(z.tine.TineId,0) < min){
                min = r.positions.getOrElse(z.tine.TineId,0)
                r_1 = r
                z_1 = z
              }
            }
          }
        }

        if(r_1.positions.isEmpty && z_1.positions.isEmpty){
          for(r <- R){
            for(z <- Z){
              if(r.positions.getOrElse(z.tine.TineId,0) < min){
                min = r.positions.getOrElse(z.tine.TineId,0)
                r_1 = r
                z_1 = z
              }
            }
          }
        }


        //extend two tines
        if(r_1.tine.TineId == z_1.tine.TineId){   // creation of a new tine

          val tineNew: Tine = Tine(rnd.nextInt(),z_1.tine.gap,z_1.tine.reserve,z_1.tine.reach,copyBlocks(z_1.tine.blocks))
          val r_new: Prefixes = Prefixes(tineNew, copyPositions(z_1.positions))
          //update r_1 id to make it a new tine
          r_new.tine.TineId = rnd.nextInt()
          r_new.tine.blocks += (r_new.tine.blocks .size+1 -> newBlock)
          r_new.positions += (z_1.tine.TineId -> t)

          fork += (t -> r_new)

          // update the slot where the fork begins
          val newBlockTwo: Block = Block(rnd.nextInt(), t, t, t, t, rnd.nextInt())
          for(tine <- fork){
            if(tine._2.tine.TineId == z_1.tine.TineId){
              tine._2.positions += (r_new.tine.TineId -> t)
              tine._2.tine.blocks += (tine._2.tine.blocks.size+1 -> newBlockTwo)
            }
            else if (tine._2.tine.TineId  != r_new.tine.TineId){
              tine._2.positions += (r_new.tine.TineId -> z_1.positions.getOrElse(tine._2.tine.TineId,0))
            }
          }
        } else {
          for(tine <- fork){
            if(tine._2.tine.TineId == r_1.tine.TineId){
              r_1.tine.blocks += (r_1.tine.blocks.size+1 -> newBlock)
            }
            if(tine._2.tine.TineId == z_1.tine.TineId){
              val newBlockTwo: Block = Block(rnd.nextInt(), t, t, t, t, rnd.nextInt())
              z_1.tine.blocks += (z_1.tine.blocks.size+1 ->newBlockTwo)
            }
          }
        }
        updateGap()
        updateReserve(t,w)
      }
    }
  }

  /*if (true) {
    val w = "hAhAhHAAH"
    //Initilization
    val newBlockInit: Block = Block(rnd.nextInt(), 0, 0, 0, 0, rnd.nextInt())
    val temp:mutable.Map[Int,Block] =  mutable.Map(0 -> newBlockInit)
    val tine: Tine = Tine(rnd.nextInt(),0,0,0,temp)
    val tineWithPrefixes: Prefixes = Prefixes(tine, mutable.Map.empty)

    fork = mutable.Map(0 -> tineWithPrefixes)

    createFork(w)

  }*/

  println(
    "*************************************************************************************"+
      "\n Settlement Game Simulation"+
      "\n*************************************************************************************"
  )

  var wt = ""
  var wT = ""
  //Initilization
  val newBlockInit: Block = Block(rnd.nextInt(), 0, 0, 0, 0, rnd.nextInt())
  val temp:mutable.Map[Int,Block] =  mutable.Map(0 -> newBlockInit)
  val tine: Tine = Tine(rnd.nextInt(),0,0,0,temp)
  val tineWithPrefixes: Prefixes = Prefixes(tine, mutable.Map.empty)

  fork = mutable.Map(0 -> tineWithPrefixes)


  //start the simulation
  for (t <- 1 to T) {

    /**
      * Honest activity is represented here at the beginning of each round
      * Challengers chain select and deliver blocks to one another in order with a forced delay in slots
      * Every Delta-divergence is forced to
      */
    for ((id,delay) <- Random.shuffle(deliverySchedule.toList)) {
      if (delay > 0) {
        deliverySchedule.update(id,delay-1) //decrement delay
      } else {
        deliverySchedule.remove(id)
        blockResponses.get(id) match {
          case None =>
            challengers.foreach(h => h.chainSelect(blockDb(id))) //deliver blocks
            val challengerHeads:Set[Int] = challengers.map(h => h.head).toSet
            if (challengerHeads.size == 1) {
              uniqueSlots.update(t,challengerHeads.map(blockDb(_)).toList)
            } else {
              forkedSlots.update(t,challengerHeads.map(blockDb(_)).toList)
            }
          case Some(rid) => //segment the network in 2, either due to honest tie, or player delivery of block response
            assert(blockDb(id).n == blockDb(rid).n)
            challengers.take(challengers.length/2).foreach(h => h.chainSelect(blockDb(id)))
            challengers.takeRight(challengers.length/2).foreach(h => h.chainSelect(blockDb(rid)))
            val challengerHeads:Set[Int] = challengers.map(h => h.head).toSet
            if (challengerHeads.size == 1) {
              uniqueSlots.update(t,challengerHeads.map(blockDb(_)).toList)
            } else {
              forkedSlots.update(t,challengerHeads.map(blockDb(_)).toList)
            }
        }

      }
    }

    //challengers make blocks
    val challengerBlocks = challengers.map(h => h.test(t)).filter(_.isDefined).map(updateDatabase).toList
    //challengers diffuse blocks with a fixed delay
    for (block <- challengerBlocks) {
      deliverySchedule.update(block.id,bounded_delay)
    }

    //get the set of distinct ids corresponding to the head of the chain among all challengers
    //size of this set is the number of forks or diverging node views among the challengers
    val challengerHeads:Set[Int] = challengers.map(h => h.head).toSet

    val staticAdversaryBlocks:List[Block] = players.map(a => a.test(t,challengerHeads.head)).filter(_.isDefined) match {
      //static adversary creates at least two blocks with each leadership eligibility
      case blocks if blocks.length >= 2 => blocks.map(updateDatabase).toList
      case blocks if blocks.length == 1 =>
        val b = blocks.head.get
        List(
          Some(b),
          Some(copyBlock(b))
        ).map(updateDatabase)
      case _ =>
        List()
    }

    if (staticAdversaryBlocks.nonEmpty) covertBlocks ::= staticAdversaryBlocks.head
    if (challengerBlocks.nonEmpty) honestBlocks = honestBlocks ++ challengerBlocks

    //player reacts to new blocks
    (challengerBlocks.nonEmpty,staticAdversaryBlocks.nonEmpty) match {
      /**
        * Empty trials, perpendicular symbol
        */
      case (false,false) => //player does nothing
      /**
        * Unique honest trials, h symbol
        */
      case (true,false) if challengerBlocks.size == 1 => // unique block
        wt ++= "h"
        wT ++= "h"
        println("h")
        //update latest unique honest block
        val latestBlock = challengerBlocks.head

        /*val bestTineId = getLongestTine()

        for(tine <- fork){
          if(tine._2.tine.TineId == bestTineId){
            tine._2.tine.blocks += (t+1 -> latestBlock)
          }
        }
        updateGap()
        updateReserve(t,wt)*/





        val leadBlockNumber = blockDb(leadingBlock).n
        if (leadBlockNumber < latestBlock.n) {
          leadingBlock = challengerBlocks.head.id
          covertBlocks.find(b => b.n == latestBlock.n) match {
            case Some(block) =>
              blockResponses.update(latestBlock.id,block.id)
              k += 1
            case None =>
              // Attempt to construct a maximum depth tine from all blocks in all branches
              settlements ::= k
              k = 1
              s = t
              xid = leadingBlock
              covertBlocks = List.empty
              honestBlocks = List.empty
              testCache = mutable.Map.empty
              thrCache = mutable.Map.empty
          }
        }

      /**
        * Honest ties, H symbol:
        */
      case (true,false) if challengerBlocks.size > 1 =>
        wt ++= "H"
        wT ++= "H"
        println("H")
        //two of the honest blocks are scheduled to be delivered together so the challengers are forked
        blockResponses.update(challengerBlocks.head.id,challengerBlocks(1).id)
        k += 1


        /*val Z: mutable.Set[Prefixes] = getZeroReachSet()
        val R: mutable.Set[Prefixes] = getMaxReachSet()
        //val (Z,R) = getZeroReachandMaxReachSets()
        // find r_1 and z_1
        var min = 99999999
        val newBlock: Block = Block(rnd.nextInt(), t, t, t, t, rnd.nextInt())
        val temp:mutable.Map[Int,Block] =  mutable.Map(t -> newBlock)
        val tine: Tine = Tine(rnd.nextInt(),0,0,0,temp)
        val tineWithPrefixes = Prefixes(tine, mutable.Map.empty)
        var r_1: Prefixes = Prefixes(tine, mutable.Map.empty)
        var z_1: Prefixes = Prefixes(tine, mutable.Map.empty)

        for(r <- R){
          for(z <- Z){
            // prefer having two different tines with two different head
            if(r.tine.TineId != z.tine.TineId){
              if(r.positions.getOrElse(z.tine.TineId,0) < min){
                min = r.positions.getOrElse(z.tine.TineId,0)
                r_1 = r
                z_1 = z
              }
            }
          }
        }

        if(r_1.positions.isEmpty && z_1.positions.isEmpty){
          for(r <- R){
            for(z <- Z){
              if(r.positions.getOrElse(z.tine.TineId,0) < min){
                min = r.positions.getOrElse(z.tine.TineId,0)
                r_1 = r
                z_1 = z
              }
            }
          }
        }

        //extend two tines
        if(r_1.tine.TineId == z_1.tine.TineId){   // creation of a new tine
          val tineNew: Tine = Tine(rnd.nextInt(),z_1.tine.gap,z_1.tine.reserve,z_1.tine.reach,copyBlocks(z_1.tine.blocks))
          val r_new: Prefixes = Prefixes(tineNew, copyPositions(z_1.positions))
          //update r_1 id to make it a new tine
          r_new.tine.TineId = rnd.nextInt()
          r_new.tine.blocks += (t+1 -> newBlock)
          r_new.positions += (z_1.tine.TineId -> t)

          fork += (t -> r_new)

          // update the slot where the fork begins
          val newBlockTwo: Block = Block(rnd.nextInt(), t, t, t, t, rnd.nextInt())
          for(tine <- fork){
            if(tine._2.tine.TineId == z_1.tine.TineId){
              tine._2.positions += (r_new.tine.TineId -> t)
              tine._2.tine.blocks += (t+1 -> newBlockTwo)
            }
            else if (tine._2.tine.TineId  != r_new.tine.TineId){
              tine._2.positions += (r_new.tine.TineId -> z_1.positions.getOrElse(tine._2.tine.TineId,0))
            }
          }
        } else {
          for(tine <- fork){
            if(tine._2.tine.TineId == r_1.tine.TineId){
              r_1.tine.blocks += (t+1 -> newBlock)
            }
            if(tine._2.tine.TineId == z_1.tine.TineId){
              val newBlockTwo: Block = Block(rnd.nextInt(), t, t, t, t, rnd.nextInt())
              z_1.tine.blocks += (t+1 ->newBlockTwo)
            }
          }
        }

        updateGap()
        updateReserve(t,wt)*/

      /**
        * Adversarial leader elected, A symbol: player may construct arbitrary forks covertly
        */
      case _ =>
        wt ++= "A"
        wT ++= "A"
        println("A")
    }
    if(wt.length() == gamma || t == T){
      println(wt)
      createFork(wt)
      wt = ""
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

  println(
    "*************************************************************************************"+
      "\n Settlement Game Simulation:"+
      "\n Challengers are honestly behaving automata"+
      "\n Players are adaptive adversaries attempting to execute a balanced fork attack"+
      "\n*************************************************************************************"
  )

  println(s"Player Score:")
  println(s"Proportion of adversarial blocks on dominant tine: ${numAdvBlocks.toDouble/maxBlockNumber}")
  println(s"Proportion of adversarial stake: ${players.map(_.alpha).sum}")
  println(s"Proportion of unique slots p_0/(p_0+p_1) = ${uniqueSlots.keySet.size.toDouble/(uniqueSlots.keySet.size+forkedSlots.keySet.size)}")
  println(s"Expectation of settlement depth in blocks: ${settlements.sum.toDouble/settlements.size}")


  println("Number of tines in batches: "+fork.size)

  var TineId = getLongestTine()

  for(tine <- fork){
    if(tine._2.tine.TineId == TineId){
      println("Length of longest tine: "+tine._2.tine.blocks.size)

    }
  }
  /*for(tine <- fork){
    println("Tine: "+tine._2.tine.blocks.toSeq.sortWith(_._1 < _._1))
    //println("Tine: "+tine._2.tine)
    println("Position: "+tine._2.positions)
  }*/


  // Create a characteristic string, then form a fork
  fork = mutable.Map(0 -> tineWithPrefixes)
  println("Characteristic String: "+wT)
  createFork(wT)

  println("Number of tines in one run: "+fork.size)

  val TineId2 = getLongestTine()
  for(tine <- fork){
    if(tine._2.tine.TineId == TineId2){
      println("Length of longest tine: "+tine._2.tine.blocks.size)

    }
  }
}

object SettlementGameSimTaktikos extends App {
  new SettlementGameSimTaktikos
}
