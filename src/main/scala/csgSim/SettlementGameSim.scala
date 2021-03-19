package csgSim

import scala.collection.mutable
import scala.util.Random

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

class SettlementGameSim{

  //generates random IDs for blocks
  val rnd:Random = new Random
  var fork: mutable.Map[Int, Prefixes] = mutable.Map.empty


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
                 val tine: Tine,
                 var positions: mutable.Map[Int, Int]
                 )




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
        Z += (tine._2)
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
        R += tine._2
      }
    }

    R
  }
  def updateGap() {
    val bestTineId = getLongestTine()
    var maxTine = 0
    for(tine <- fork){
      if(tine._2.tine.TineId == bestTineId){
        maxTine = tine._2.tine.blocks.size
      }

    }

    for(tine <- fork){

      tine._2.tine.gap = maxTine - tine._2.tine.blocks.size
    }

  }

  def updateReserve(currSlot: Int, charString: String){
    var reserve = 0
    for(i <- currSlot to charString.length-1){
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







  val w = "hAhAhHAAH"

  for (t <- 0 to w.length-1){

      val newBlock: Block = Block(rnd.nextInt(), t, t, t, t, rnd.nextInt())

      if(fork.size == 0){
        val temp:mutable.Map[Int,Block] =  mutable.Map(t -> newBlock)
        val tine: Tine = Tine(rnd.nextInt(),0,0,0,temp)
        val timeWithPrefixes = Prefixes(tine, mutable.Map.empty)
        fork = mutable.Map(t -> timeWithPrefixes)
      }
      else{
        if(w.toList(t).equals('h')) {
          val bestTineId = getLongestTine()


          for(tine <- fork){
            if(tine._2.tine.TineId == bestTineId){
              tine._2.tine.blocks += (t -> newBlock)
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

          val temp:mutable.Map[Int,Block] =  mutable.Map(t -> newBlock)
          val tine: Tine = Tine(rnd.nextInt(),0,0,0,temp)
          val timeWithPrefixes = Prefixes(tine, mutable.Map.empty)
          var r_1: Prefixes = Prefixes(tine, mutable.Map.empty)
          var z_1: Prefixes = Prefixes(tine, mutable.Map.empty)



          for(r <- R){
            for(z <- Z){


              if(r.positions.getOrElse(z.tine.TineId,0) < min){
                 min = r.positions.getOrElse(z.tine.TineId,0)
                 r_1 = r
                 z_1 = z
              }
            }
          }


          //extend two tines
          if(r_1.tine.TineId == z_1.tine.TineId){   // creation of a new tine

            val tine: Tine = Tine(rnd.nextInt(),0,0,0,temp)
            val timeWithPrefixes = Prefixes(tine, mutable.Map.empty)
            val r_new: Prefixes = z_1.copy()
            //update r_1 id to make it a new tine
            r_new.tine.TineId = rnd.nextInt()
            r_new.tine.blocks += (t -> newBlock)
            r_new.positions += (z_1.tine.TineId -> t)
            println("Size: "+fork.size)
            println("slot: "+t)
            println("New tine length: "+r_new.tine.blocks.size)

            fork += (t -> r_new)


            // update the slot where the fork begins
            for(tine <- fork){
              if(tine._2.tine.TineId == z_1.tine.TineId){
                tine._2.positions += (r_new.tine.TineId -> t)
              }
              else{
                tine._2.positions += (r_new.tine.TineId -> z_1.positions.getOrElse(tine._2.tine.TineId,0))
              }
            }
          }
          else{
            for(tine <- fork){
              if(tine._2.tine.TineId == r_1.tine.TineId){
                r_1.tine.blocks += (t -> newBlock)
              }
              if(tine._2.tine.TineId == z_1.tine.TineId){
                z_1.tine.blocks += (t ->newBlock)
              }
            }


          }

          updateGap()
          updateReserve(t,w)


        }
      }


  }

  println(
    "*************************************************************************************"+
      "\n Settlement Game Simulation:"+
      "\n*************************************************************************************"
  )
  println("Number of tines: "+fork.size)

}

object SettlementGameSim extends App {
  new SettlementGameSim
}
