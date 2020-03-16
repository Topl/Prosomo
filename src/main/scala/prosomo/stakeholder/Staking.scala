package prosomo.stakeholder

import bifrost.crypto.hash.FastCryptographicHash
import prosomo.components.Tine
import prosomo.primitives.{Ratio, SharedData}

import scala.math.BigInt

trait Staking extends Members {

  import prosomo.primitives.Parameters._

  /**
    * Aggregate staking function used for calculating threshold per epoch
    * @param a relative stake
    * @return probability of being elected slot leader
    */
  def phi(a:Ratio): Ratio = {
    var out = Ratio(0)
    val base = maclaurin_coefficient * a
    for (n <- 1 to o_n) {
      out = out - ( base.pow(n) / factorial(n) )
    }
    //    if (holderIndex == 0) {
    //val alpha = a.toBigDecimal.toDouble
    //      val phiDouble = 1.0 - scala.math.pow(1.0 - f_s,alpha)
    //      println(a.toString)
    //      println(maclaurin_coefficient.toString)
    //      println(s"alpha double:$alpha")
    //      println(s"phi double:$phiDouble")
    //      println(s"phi Ratio :${out.toBigDecimal.toDouble}")
    //    }
    out
  }

  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n-1)
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
    //    if (holderIndex == 0) {
    //      println(s"net:${net.toBigDecimal.toDouble}")
    //      println(s"thr:${t.toBigDecimal.toDouble}")
    //      println(net < t)
    //    }
    net < t
  }

  /**
    * calculates alpha, the epoch relative stake, from the staking state
    * @param holderKey
    * @param ls
    * @return
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
        case b:BlockHeader => b._1.data
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
      FastCryptographicHash(eta_from_genesis(next,ep-1)++serializer.getBytes(ep)++v)
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
    //println(s"Holder $holderIndex:eta in:"+Base58.encode(etaP))
    if(ep == 0) {
      getBlockHeader(c.get(0)) match {
        case b:BlockHeader => b._1.data
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
      val eta_ep = FastCryptographicHash(eta_prev++serializer.getBytes(ep)++v)
      //println(s"Holder $holderIndex:eta out:"+Base58.encode(eta_ep))
      eta_ep
    }
  }


}
