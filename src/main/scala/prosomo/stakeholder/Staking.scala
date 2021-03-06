package prosomo.stakeholder

import prosomo.components.Tine
import prosomo.primitives.{Ratio, SharedData}
import com.google.common.cache.{CacheBuilder, CacheLoader}
import scorex.util.encode.Base58

import scala.math.BigInt

/**
  * AMS 2020:
  * Mathematical methods required for the Staking Procedure described in Ouroboros Genesis
  */

trait Staking extends Members {

  /**
    * Staking function used for calculating threshold per epoch, fixed difficulty
    * @param a relative stake
    * @return probability of being elected slot leader
    */

  def phi(a:Ratio): Ratio = {
    var out = Ratio(0)
    val base = m_f * a
    for (n <- 1 to o_n) {
      out = out - ( base.pow(n) * Ratio(BigInt(1),factorial_cache(n)) )
    }
    out
  }

  /**
    * Staking function used for calculating threshold per epoch, dynamic difficulty
    * @param a relative stake
    * @param m_f coefficient log(1-f(slot-parentSlot))
    * @return probability of being elected slot leader
    */

  def phi(a:Ratio,m_f:Ratio): Ratio = {
    var out = Ratio(0)
    val base = m_f * a
    for (n <- 1 to o_n) {
      out = out - ( base.pow(n) * Ratio(BigInt(1),factorial_cache(n)) )
    }
    out
  }

  /**
    * Staking function parameterized in terms of slot and parent slot, dynamic difficulty
    * @param a relative stake
    * @param s_interval delta between slot of header and slot of parent
    * @return probability of being elected slot leader
    */

  def threshold(a:Ratio, s_interval:Slot):Ratio = {
    assert(s_interval>0)
    s_interval match {
      case int:Int if int < m_f_range.length =>
        phi(a,m_f_range(int))
      case _ =>
        phi(a,m_f_B)
    }
  }

  def threshold_cached(a:Ratio, s_interval:Slot):Ratio = {
    thresholdCache match {
      case None =>
        thresholdCache = Some(CacheBuilder.newBuilder()
          .maximumSize(1000)
          .build[(Ratio,Slot),Ratio](
            new CacheLoader[(Ratio,Slot),Ratio] {
              def load(id:(Ratio,Slot)):Ratio = {
                threshold(id._1,id._2)
              }
            }
          ))
      case _ =>
    }
    s_interval match {
      case int:Int if int < m_f_range.length =>
        thresholdCache.get.get((a,s_interval))
      case _ =>
        thresholdCache.get.get((a,m_f_range.length))
    }
  }

  def baseSlot(s:Slot):Slot = if(useStableIntervalTerm) {
    s match {
      case _ if s <= 0 => s
      case _ => forging_window*(s/forging_window+1)
    }
  } else {
    s
  }

  val factorial_cache:Array[BigInt] = (0 to o_n).toArray.map(i=>factorial(i))

  def factorial(n: Int): BigInt = n match {
    case 0 => BigInt(1)
    case _ => BigInt(n) * factorial(n-1)
  }

  /**
    * Compares the vrf output to the threshold
    * @param y vrf output bytes
    * @param t threshold between 0.0 and 1.0
    * @return true if y mapped to real number between 0.0 and 1.0 is less than threshold
    */
  def compare(y: Array[Byte],t: Ratio):Boolean = {
    var net:Ratio = Ratio(0)
    var i:Int = 0
    for (byte <- y){
      i += 1
      val n = BigInt(byte & 0xff)
      val d = BigInt(2).pow(8*i)
      net = net + new Ratio(n,d)
    }
    net < t
  }

  /**
    * Test strategy for threshold comparison,
    * y is the vrf slot commitment generated by the forging party,
    * ps is the parent slot, slot of the head that was forged on,
    * bn is the block number of the header being tested,
    * The idea behind different strategies is that local predictability may be thwarted by making the test value
    * harder to predict by adding extra parameters while choosing parameters that are hard to change,
    * The arguments chosen correlate to the chain quality of the head that is being forged on so grinding attacks
    * can only be carried out by reducing the quality of the malicious tine that is being constructed
    * inherently making that tine less viable
    */

  def stakingTestStrategy(y:Rho,ps:Slot,bn:Int,rho:Rho,s_interval:Slot):Rho = testStrategy match {
    case "vrf" => y
    case "parent-slot-hash" => Sha512(y++serializer.getBytes(ps))
    case "parent-slot-number-hash" => Sha512(y++serializer.getBytes(ps)++serializer.getBytes(bn))
    case "taktikos-bounded-header-nonce" =>
      if (s_interval <= gamma) {
        Sha512(y++rho)
      } else {
        y
      }
  }

  /**
    * calculates alpha, the epoch relative stake, from the staking state
    * @param holderKey the holder public address
    * @param ls state from which the relative stake is calculated
    * @return state allocated to holderKey divided by all stake in state
    */
  def relativeStake(holderKey:PublicKeyW,ls:State): Ratio = {
    var netStake:BigInt = 0
    var holderStake:BigInt = 0
    for (member <- ls.keySet) {
      val (balance,activityIndex, _) = ls(member)
      if (activityIndex) netStake += balance
    }
    if (ls.keySet.contains(holderKey)){
      val (balance,activityIndex, _) = ls(holderKey)
      if (activityIndex) holderStake += balance
    }
    if (netStake > 0) {
      new Ratio(holderStake,netStake)
    } else {
      new Ratio(BigInt(0),BigInt(1))
    }
  }

  /**
    * calculates epoch nonce recursively
    * @param c local chain to be verified
    * @param ep epoch derived from time step
    * @return hash nonce
    */
  def eta_from_genesis(c:Tine, ep:Int): Eta = {
    if(ep == 0) {
      getBlockHeader(c.get(0).get) match {
        case Some(b:BlockHeader) => b._1.data
        case _ =>
          println("error: ep 0 eta not recovered")
          SharedData.throwError(holderIndex)
          Array()
      }
    } else {
      val prev_two_thirds_epoch:Array[Byte] =
        c.orderedNonceData((ep-1)*epochLength,ep*epochLength-epochLength/3-1,None)
      assert(prev_two_thirds_epoch.nonEmpty)
      fch.hash(eta_from_genesis(c,ep-1) ++ serializer.getBytes(ep) ++ prev_two_thirds_epoch)
    }
  }

  /**
    * calculates epoch nonce from previous nonce
    * @param chain local chain
    * @param ep epoch derived from time step
    * @param eta_prev previous eta
    * @param tine optional tine argument for validation
    * @return hash nonce
    */
  def eta_from_tine(chain:Tine, ep:Int, eta_prev:Eta, tine:Option[Tine]): Eta = {
    if(ep == 0) {
      getBlockHeader(chain.get(0).get) match {
        case Some(b:BlockHeader) => b._1.data
        case _ =>
          println("error: ep 0 eta not recovered")
          SharedData.throwError(holderIndex)
          Array()
      }
    } else {
      val prev_two_thirds_epoch:Array[Byte] =
        chain.orderedNonceData((ep-1)*epochLength,ep*epochLength-epochLength/3-1,tine)
      val eta_ep = fch.hash(eta_prev ++ serializer.getBytes(ep) ++ prev_two_thirds_epoch)
      eta_ep
    }
  }
}
