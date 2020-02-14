package prosomo.components

import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives.{Parameters, Sig,Ratio}
import prosomo.components.Types._
import scala.math.BigInt

case class Transaction(sender:PublicKeyW,receiver:PublicKeyW,delta:BigInt,sid:Sid,nonce:Int,signature: Signature) {

  def verify(sig:Sig,serializer: Serializer): Boolean = {
    sig.verify(signature,receiver.data++delta.toByteArray++sid.data++serializer.getBytes(nonce),sender.data.take(sig.KeyLength))
  }

  /**
    * applies an individual transaction to state
    * @param ls old local state to be updated
    * @param forger sig public key of the forger
    * @return updated localstate
    */
  def applyTransaction(ls:State, forger:PublicKeyW, fee_r:Ratio): Any = {
    {
      var nls:State = ls
      val validSender = nls.keySet.contains(sender)
      val txC_s:Int = nls(sender)._3
      if (nonce != txC_s) println(nonce,txC_s)
      if (validSender && nonce == txC_s) {
        //val fee = BigDecimal(delta.toDouble*transactionFee).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        val fee:BigInt = (Ratio(delta)*fee_r).round
        val validRecip = nls.keySet.contains(receiver)
        val validFunds = nls(sender)._1 >= delta
        if (validRecip && validFunds) {
          if (sender == receiver && sender != forger) {
            val s_net:BigInt = nls(sender)._1
            val f_net:BigInt = nls(forger)._1
            val f_txC:Int = nls(forger)._3
            val s_new: BigInt = s_net - fee
            val f_new: BigInt = f_net + fee
            nls -= sender
            nls -= forger
            nls += (sender -> (s_new,true,nonce+1))
            nls += (forger -> (f_new,true,f_txC))
          } else if (sender == forger) {
            val s_net:BigInt = nls(sender)._1
            val r_net:BigInt = nls(receiver)._1
            val r_txC:Int = nls(receiver)._3
            val s_new: BigInt = s_net - delta + fee
            val r_new: BigInt = r_net + delta - fee
            nls -= sender
            nls -= receiver
            nls += (sender -> (s_new,true,nonce+1))
            nls += (receiver -> (r_new,true,r_txC))
          } else if (receiver == forger) {
            val s_net:BigInt = nls(sender)._1
            val r_net:BigInt = nls(receiver)._1
            val r_txC:Int = nls(receiver)._3
            val s_new: BigInt = s_net - delta
            val r_new: BigInt = r_net + delta
            nls -= sender
            nls -= receiver
            nls += (sender -> (s_new,true,nonce+1))
            nls += (receiver -> (r_new,true,r_txC))
          } else if (!nls.keySet.contains(forger)) {
            val s_net:BigInt = nls(sender)._1
            val r_net:BigInt = nls(receiver)._1
            val r_txC:Int = nls(receiver)._3
            val s_new: BigInt = s_net - delta
            val r_new: BigInt = r_net + delta - fee
            nls -= sender
            nls -= receiver
            nls += (sender -> (s_new,true,nonce+1))
            nls += (receiver -> (r_new,true,r_txC))
          } else {
            val s_net:BigInt = nls(sender)._1
            val r_net:BigInt = nls(receiver)._1
            val r_txC:Int = nls(receiver)._3
            val f_net:BigInt = nls(forger)._1
            val f_txC:Int = nls(forger)._3
            val s_new: BigInt = s_net - delta
            val r_new: BigInt = r_net + delta - fee
            val f_new: BigInt = f_net + fee
            nls -= sender
            nls -= receiver
            nls -= forger
            nls += (sender -> (s_new,true,nonce+1))
            nls += (receiver -> (r_new,true,r_txC))
            nls += (forger -> (f_new,true,f_txC))
          }
          nls
        } else if (validFunds) {
          if (sender == forger) {
            val s_net:BigInt = nls(sender)._1
            val r_net:BigInt = 0
            val s_new: BigInt = s_net - delta + fee
            val r_new: BigInt = r_net + delta - fee
            nls -= sender
            nls += (sender -> (s_new,true,nonce+1))
            nls += (receiver -> (r_new,true,0))
          } else if (!nls.keySet.contains(forger)) {
            val s_net:BigInt = nls(sender)._1
            val r_net:BigInt = 0
            val s_new: BigInt = s_net - delta
            val r_new: BigInt = r_net + delta - fee
            nls -= sender
            nls += (sender -> (s_new,true,nonce+1))
            nls += (receiver -> (r_new,true,0))
          } else {
            val s_net:BigInt = nls(sender)._1
            val r_net:BigInt = 0
            val f_net:BigInt = nls(forger)._1
            val f_txC = nls(forger)._3
            val s_new: BigInt = s_net - delta
            val r_new: BigInt = r_net + delta - fee
            val f_new: BigInt = f_net + fee
            nls -= sender
            nls -= forger
            nls += (sender -> (s_new,true,nonce+1))
            nls += (receiver -> (r_new,true,0))
            nls += (forger -> (f_new,true,f_txC))
          }
          nls
        }
      } else {
        0
      }
    }
  }

}
