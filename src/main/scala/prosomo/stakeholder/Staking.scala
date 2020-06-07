package prosomo.stakeholder

import prosomo.components.Tine
import prosomo.primitives.{Ratio, SharedData}
import com.google.common.cache.{CacheBuilder, CacheLoader}
import scala.math.BigInt

/**
  * AMS 2020:
  * Mathematical methods required for the Staking Procedure described in Ouroboros Genesis
  */

trait Staking extends Members {

  import prosomo.primitives.Parameters._

  /**
    * Aggregate staking function used for calculating threshold per epoch, fixed difficulty
    * @param a relative stake
    * @return probability of being elected slot leader
    */
  def phi(a:Ratio): Ratio = {
    var out = Ratio(0)
    val base = m_f_root * a
    for (n <- 1 to o_n) {
      out = out - ( base.pow(n) * Ratio(BigInt(1),factorial_cache(n)) )
    }
    out
  }

  /**
    * Aggregate staking function used for calculating threshold per epoch, dynamic difficulty
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
    * Aggregate staking function parameterized in terms of slot and parent slot, dynamic difficulty
    * @param a relative stake
    * @param s_interval delta between slot of header and slot of parent
    * @return probability of being elected slot leader
    */

  def threshold(a:Ratio, s_interval:Slot):Ratio = {
    val index:Int = s_interval-1 match {
      case int: Int if int < m_f_range.length => int
      case _ => m_f_range.length-1
    }
    phi(a,m_f_range(index))
  }

  def threshold_cached(a:Ratio, s_interval:Slot):Ratio = {
    val index:Int = s_interval-1 match {
      case int: Int if int < m_f_range.length => int
      case _ => m_f_range.length-1
    }
    thresholdCache match {
      case None => {
        thresholdCache = Some(CacheBuilder.newBuilder()
          .maximumSize(1000)
          .build[(Ratio,Slot),Ratio](
          new CacheLoader[(Ratio,Slot),Ratio] {
            def load(id:(Ratio,Slot)):Ratio = {phi(id._1,m_f_range(id._2))}
          }
        ))
      }
      case _ =>
    }
    thresholdCache.get.get((a,index))
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
    * @return true if y mapped to double between 0.0 and 1.0 is less than threshold
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
    * calculates alpha, the epoch relative stake, from the staking state
    * @param holderKey the holder public address
    * @param ls state from which the relative stake is calculated
    * @return state allocated to holderKey divided by all stake in state
    */
  def relativeStake(holderKey:PublicKeyW,ls:State): Ratio = {
    var netStake:BigInt = 0
    var holderStake:BigInt = 0
    for (member <- ls.keySet) {
      val (balance,activityIndex,txC) = ls(member)
      if (activityIndex) netStake += balance
    }
    if (ls.keySet.contains(holderKey)){
      val (balance,activityIndex,txC) = ls(holderKey)
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
      getBlockHeader(c.get(0)) match {
        case Some(b:BlockHeader) => b._1.data
        case _ => {
          println("error: ep 0 eta not recovered")
          SharedData.throwError(holderIndex)
          Array()
        }
      }
    } else {
      var v: Array[Byte] = Array()
      val prev_two_thirds_epoch = subChain(c,ep*epochLength-epochLength,ep*epochLength-epochLength/3)
      for(id <- prev_two_thirds_epoch.ordered) {
        v = v++prev_two_thirds_epoch.getNonce(id._1)
      }
      val next = subChain(c,0,ep*epochLength-epochLength)
      fch.hash(eta_from_genesis(next,ep-1)++serializer.getBytes(ep)++v)
    }
  }

  /**
    * calculates epoch nonce from previous nonce
    * @param c local chain to be verified
    * @param ep epoch derived from time step
    * @param eta_prev previous eta
    * @return hash nonce
    */
  def eta_from_tine(c:Tine, ep:Int, eta_prev:Eta): Eta = {
    if(ep == 0) {
      getBlockHeader(c.get(0)) match {
        case Some(b:BlockHeader) => b._1.data
        case _ => {
          println("error: ep 0 eta not recovered")
          SharedData.throwError(holderIndex)
          Array()
        }
      }
    } else {
      var v: Array[Byte] = Array()
      val prev_two_thirds_epoch = subChain(c,ep*epochLength-epochLength,ep*epochLength-epochLength/3)
      for(id <- prev_two_thirds_epoch.ordered) {
        v = v++prev_two_thirds_epoch.getNonce(id._1)
      }
      val eta_ep = fch.hash(eta_prev++serializer.getBytes(ep)++v)
      eta_ep
    }
  }


}
