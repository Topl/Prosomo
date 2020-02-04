package prosomo

import scala.language.postfixOps
import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper

import scala.math.BigInt
import scala.util.Random

trait Functions
  extends Types
    with Parameters
    with Utils {

  /**
    * retrieve parent block id from block
    * @param b
    * @return parent id
    */
  def getParentId(b:Block): BlockId = {
    (b._10,b._1)
  }

  /**
    * finds the last non-empty slot in a chain
    * @param c chain of block ids
    * @param s slot to start search
    * @return last active slot found on chain c starting at slot s
    */
  def lastActiveSlot(c:Chain,s:Slot): Slot = {
    var i = s
    while (c(i)._2.data.isEmpty) {
      i-=1
    }
    i
  }

  /**
    * returns the total number of active slots on a chain
    * @param c chain of block ids
    * @return total active slots
    */
  def getActiveSlots(c:Chain): Int = {
    var i = 0
    for (id<-c) {
      if (!id._2.data.isEmpty) {
        i+=1
      }
    }
    i
  }

  /**
    * main hash routine used in prosomo
    * @param input any bytes
    * @return wrapped byte array
    */
  def hash(input:Any): ByteArrayWrapper = {
    ByteArrayWrapper(FastCryptographicHash(serialize(input)))
  }

  /**
    * returns a sub-chain containing all blocks in a given time interval
    * @param c input chain
    * @param t1 slot lower bound
    * @param t2 slot upper bound
    * @return all blocks in the interval t1 to t2, including blocks of t1 and t2
    */
  def subChain(c:Chain,t1:Int,t2:Int): Chain = {
    var t_lower:Int = 0
    var t_upper:Int = 0
    if (t1>0) t_lower = t1
    if (t2>0) t_upper = t2
    c.slice(t_lower,t_upper+1)
  }

  /**
    * expands a tine to have empty slots in between active slots
    * @param c dense chain
    * @param p prefix slot
    * @return expanded tine
    */
  def expand(c:Chain,p:Slot,s:Slot): Chain ={
    val out = Array.fill(s-p){(-1,ByteArrayWrapper(Array()))}
    for (id <- c) {
      out.update(id._1-p-1,id)
    }
    assert(out.length == s-p)
    out
  }

  /**
    * Aggregate staking function used for calculating threshold per epoch
    * @param a relative stake
    * @param f active slot coefficient
    * @return probability of being elected slot leader
    */
  def phi(a:Double,f:Double): Double = {
    1.0 - scala.math.pow(1.0 - f,a)
  }

  /**
    * Compares the vrf output to the threshold
    * @param y vrf output bytes
    * @param t threshold between 0.0 and 1.0
    * @return true if y mapped to double between 0.0 and 1.0 is less than threshold
    */
  def compare(y: Array[Byte],t: Double):Boolean = {
    var net = 0.0
    var i =0
    for (byte<-y){
      i+=1
      val n = BigInt(byte & 0xff).toDouble
      val norm = scala.math.pow(2.0,8.0*i)
      net += n/norm
    }
    net<t
  }

  /**
    * calculates alpha, the epoch relative stake, from the staking state
    * @param holderKey
    * @param ls
    * @return
    */
  def relativeStake(holderKey:PublicKeyW,ls:State): Double = {
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
      holderStake.toDouble / netStake.toDouble
    } else {
      0.0
    }
  }

  /**
    * applies an individual transaction to local state
    * @param ls old local state to be updated
    * @param trans transaction to be applied
    * @param pk_f sig public key of the forger
    * @return updated localstate
    */
  def applyTransaction(ls:State, trans:Transaction, pk_f:PublicKeyW): Any = {
    var nls:State = ls
    val pk_s:PublicKeyW = trans._1
    val pk_r:PublicKeyW = trans._2
    val validSender = nls.keySet.contains(pk_s)
    val txC_s:Int = nls(pk_s)._3
    if (trans._5 != txC_s) println(trans._5,txC_s)
    if (validSender && trans._5 == txC_s) {
      val delta:BigInt = trans._3
      val fee = BigDecimal(delta.toDouble*transactionFee).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
      val validRecip = nls.keySet.contains(pk_r)
      val validFunds = nls(pk_s)._1 >= delta
      if (validRecip && validFunds) {
        if (pk_s == pk_r && pk_s != pk_f) {
          val s_net:BigInt = nls(pk_s)._1
          val f_net:BigInt = nls(pk_f)._1
          val f_txC:Int = nls(pk_f)._3
          val s_new: BigInt = s_net - fee
          val f_new: BigInt = f_net + fee
          nls -= pk_s
          nls -= pk_f
          nls += (pk_s -> (s_new,true,trans._5+1))
          nls += (pk_f -> (f_new,true,f_txC))
        } else if (pk_s == pk_f) {
          val s_net:BigInt = nls(pk_s)._1
          val r_net:BigInt = nls(pk_r)._1
          val r_txC:Int = nls(pk_r)._3
          val s_new: BigInt = s_net - delta + fee
          val r_new: BigInt = r_net + delta - fee
          nls -= pk_s
          nls -= pk_r
          nls += (pk_s -> (s_new,true,trans._5+1))
          nls += (pk_r -> (r_new,true,r_txC))
        } else if (pk_r == pk_f) {
          val s_net:BigInt = nls(pk_s)._1
          val r_net:BigInt = nls(pk_r)._1
          val r_txC:Int = nls(pk_r)._3
          val s_new: BigInt = s_net - delta
          val r_new: BigInt = r_net + delta
          nls -= pk_s
          nls -= pk_r
          nls += (pk_s -> (s_new,true,trans._5+1))
          nls += (pk_r -> (r_new,true,r_txC))
        } else if (!nls.keySet.contains(pk_f)) {
          val s_net:BigInt = nls(pk_s)._1
          val r_net:BigInt = nls(pk_r)._1
          val r_txC:Int = nls(pk_r)._3
          val s_new: BigInt = s_net - delta
          val r_new: BigInt = r_net + delta - fee
          nls -= pk_s
          nls -= pk_r
          nls += (pk_s -> (s_new,true,trans._5+1))
          nls += (pk_r -> (r_new,true,r_txC))
        } else {
          val s_net:BigInt = nls(pk_s)._1
          val r_net:BigInt = nls(pk_r)._1
          val r_txC:Int = nls(pk_r)._3
          val f_net:BigInt = nls(pk_f)._1
          val f_txC:Int = nls(pk_f)._3
          val s_new: BigInt = s_net - delta
          val r_new: BigInt = r_net + delta - fee
          val f_new: BigInt = f_net + fee
          nls -= pk_s
          nls -= pk_r
          nls -= pk_f
          nls += (pk_s -> (s_new,true,trans._5+1))
          nls += (pk_r -> (r_new,true,r_txC))
          nls += (pk_f -> (f_new,true,f_txC))
        }
        nls
      } else if (validFunds) {
        if (pk_s == pk_f) {
          val s_net:BigInt = nls(pk_s)._1
          val r_net:BigInt = 0
          val s_new: BigInt = s_net - delta + fee
          val r_new: BigInt = r_net + delta - fee
          nls -= pk_s
          nls += (pk_s -> (s_new,true,trans._5+1))
          nls += (pk_r -> (r_new,true,0))
        } else if (!nls.keySet.contains(pk_f)) {
          val s_net:BigInt = nls(pk_s)._1
          val r_net:BigInt = 0
          val s_new: BigInt = s_net - delta
          val r_new: BigInt = r_net + delta - fee
          nls -= pk_s
          nls += (pk_s -> (s_new,true,trans._5+1))
          nls += (pk_r -> (r_new,true,0))
        } else {
          val s_net:BigInt = nls(pk_s)._1
          val r_net:BigInt = 0
          val f_net:BigInt = nls(pk_f)._1
          val f_txC = nls(pk_f)._3
          val s_new: BigInt = s_net - delta
          val r_new: BigInt = r_net + delta - fee
          val f_new: BigInt = f_net + fee
          nls -= pk_s
          nls -= pk_f
          nls += (pk_s -> (s_new,true,trans._5+1))
          nls += (pk_r -> (r_new,true,0))
          nls += (pk_f -> (f_new,true,f_txC))
        }
        nls
      }
    } else {
      0
    }
  }

  /**
    * sign a transaction to be issued
    * @param sk_s sig private key
    * @param pk_s sig public key
    * @param pk_r sig public key of recipient
    * @param delta transfer amount
    * @param txCounter transaction number
    * @return signed transaction
    */
  def signTransaction(sk_s:PrivateKey, pk_s:PublicKeyW, pk_r:PublicKeyW, delta:BigInt, txCounter:Int,sig:Sig,rng:Random): Transaction = {
    val sid:Sid = hash(rng.nextString(64))
    val trans:Transaction = (pk_s,pk_r,delta,sid,txCounter,sig.sign(sk_s,pk_r.data++delta.toByteArray++sid.data++serialize(txCounter)))
    trans
  }


}
