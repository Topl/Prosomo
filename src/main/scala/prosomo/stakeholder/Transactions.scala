package prosomo.stakeholder

import prosomo.components.{Serializer, Transaction, State}
import prosomo.primitives.Types.{PublicKeyW, StateData}
import prosomo.primitives.{Ratio, Sig}
import scala.math.BigInt
import scala.util.{Try,Success,Failure}

/**
  * AMS 2020:
  * Tx methods required by the wallet and the Stakeholder actors
  */

trait Transactions {

  def verifyTX(tx: Transaction, sig:Sig, serializer: Serializer): Boolean = {
    sig.verify(
      tx.signature,
      tx.receiver.data
        ++tx.delta.toByteArray
        ++tx.sid.data
        ++serializer.getBytes(tx.nonce),
      tx.sender.data.take(sig.keyLength)
    )
  }

  /**
    * applies an individual transaction to state
    * @param ls local state to be updated
    * @param forgerOpt sig public key of the forger
    * @return updated localstate
    */
  def applyTransaction(tx: Transaction, ls:State, forgerOpt:Option[PublicKeyW], fee_r:Ratio): Option[State] = Try{
    val txC_s:Int = ls.get(tx.sender).get._3
    val validFunds = ls.get(tx.sender).get._1 >= tx.delta
    val fee:BigInt = (Ratio(tx.delta)*fee_r).round
    assert(tx.nonce == txC_s)
    assert(validFunds)
    forgerOpt match {
      case Some(forger) =>
        ls.get(tx.receiver) match {
          case Some(_) =>
            if (tx.sender == tx.receiver && tx.sender != forger) {
              val s_net:BigInt = ls.get(tx.sender).get._1
              val f_net:BigInt = ls.get(forger).get._1
              val f_txC:Int = ls.get(forger).get._3
              val s_new: BigInt = s_net - fee
              val f_new: BigInt = f_net + fee
              ls -= tx.sender
              ls -= forger
              ls += (tx.sender -> (s_new,true,tx.nonce+1))
              ls += (forger -> (f_new,true,f_txC))
            } else if (tx.sender == forger) {
              val s_net:BigInt = ls.get(tx.sender).get._1
              val r_net:BigInt = ls.get(tx.receiver).get._1
              val r_txC:Int = ls.get(tx.receiver).get._3
              val s_new: BigInt = s_net - tx.delta + fee
              val r_new: BigInt = r_net + tx.delta - fee
              ls -= tx.sender
              ls -= tx.receiver
              ls += (tx.sender -> (s_new,true,tx.nonce+1))
              ls += (tx.receiver -> (r_new,true,r_txC))
            } else if (tx.receiver == forger) {
              val s_net:BigInt = ls.get(tx.sender).get._1
              val r_net:BigInt = ls.get(tx.receiver).get._1
              val r_txC:Int = ls.get(tx.receiver).get._3
              val s_new: BigInt = s_net - tx.delta
              val r_new: BigInt = r_net + tx.delta
              ls -= tx.sender
              ls -= tx.receiver
              ls += (tx.sender -> (s_new,true,tx.nonce+1))
              ls += (tx.receiver -> (r_new,true,r_txC))
            } else {
              val s_net:BigInt = ls.get(tx.sender).get._1
              val r_net:BigInt = ls.get(tx.receiver).get._1
              val r_txC:Int = ls.get(tx.receiver).get._3
              val f_net:BigInt = ls.get(forger).get._1
              val f_txC:Int = ls.get(forger).get._3
              val s_new: BigInt = s_net - tx.delta
              val r_new: BigInt = r_net + tx.delta - fee
              val f_new: BigInt = f_net + fee
              ls -= tx.sender
              ls -= tx.receiver
              ls -= forger
              ls += (tx.sender -> (s_new,true,tx.nonce+1))
              ls += (tx.receiver -> (r_new,true,r_txC))
              ls += (forger -> (f_new,true,f_txC))
            }
          case None =>
            if (tx.sender == forger) {
              val s_net:BigInt = ls.get(tx.sender).get._1
              val r_net:BigInt = 0
              val s_new: BigInt = s_net - tx.delta + fee
              val r_new: BigInt = r_net + tx.delta - fee
              ls -= tx.sender
              ls += (tx.sender -> (s_new,true,tx.nonce+1))
              ls += (tx.receiver -> (r_new,true,0))
            } else {
              val s_net:BigInt = ls.get(tx.sender).get._1
              val r_net:BigInt = 0
              val f_net:BigInt = ls.get(forger).get._1
              val f_txC = ls.get(forger).get._3
              val s_new: BigInt = s_net - tx.delta
              val r_new: BigInt = r_net + tx.delta - fee
              val f_new: BigInt = f_net + fee
              ls -= tx.sender
              ls -= forger
              ls += (tx.sender -> (s_new,true,tx.nonce+1))
              ls += (tx.receiver -> (r_new,true,0))
              ls += (forger -> (f_new,true,f_txC))
            }
        }
      case None =>
        ls.get(tx.receiver) match {
          case Some(_) =>
            if (tx.sender == tx.receiver) {
              val s_net:BigInt = ls.get(tx.sender).get._1
              val s_new: BigInt = s_net - fee
              ls -= tx.sender
              ls += (tx.sender -> (s_new,true,tx.nonce+1))
            } else {
              val s_net:BigInt = ls.get(tx.sender).get._1
              val r_net:BigInt = ls.get(tx.receiver).get._1
              val r_txC:Int = ls.get(tx.receiver).get._3
              val s_new: BigInt = s_net - tx.delta
              val r_new: BigInt = r_net + tx.delta - fee
              ls -= tx.sender
              ls -= tx.receiver
              ls += (tx.sender -> (s_new,true,tx.nonce+1))
              ls += (tx.receiver -> (r_new,true,r_txC))
            }
          case None =>
            val s_net:BigInt = ls.get(tx.sender).get._1
            val r_net:BigInt = 0
            val s_new: BigInt = s_net - tx.delta
            val r_new: BigInt = r_net + tx.delta - fee
            ls -= tx.sender
            ls += (tx.sender -> (s_new,true,tx.nonce+1))
            ls += (tx.receiver -> (r_new,true,0))
        }
    }
    ls
  } match {
    case Success(value) => Some(value)
    case Failure(exception) =>
      exception.printStackTrace()
      None
  }

  /**
   * unapplies an individual transaction from state
   * @param ls old local state to be updated
   * @param forger sig public key of the forger
   * @return updated localstate
   */
  def unapplyTransaction(tx: Transaction, ls:State, forger:PublicKeyW, fee_r:Ratio): Option[State] = Try{
    val txC_s:Int = ls.get(tx.sender).get._3
    val txC_r:Int = ls.get(tx.receiver).get._3
    val fee:BigInt = (Ratio(tx.delta)*fee_r).round
    assert(txC_s == tx.nonce)
    if (txC_r > 0) {
      if (tx.sender == tx.receiver && tx.sender != forger) {
          val s_net:BigInt = ls.get(tx.sender).get._1
          val f_net:BigInt = ls.get(forger).get._1
          val f_txC:Int = ls.get(forger).get._3
          val s_new: BigInt = s_net + fee
          val f_new: BigInt = f_net - fee
          ls -= tx.sender
          ls -= forger
          ls += (tx.sender -> (s_new,true,tx.nonce-1))
          ls += (forger -> (f_new,true,f_txC))
        } else if (tx.sender == forger) {
          val s_net:BigInt = ls.get(tx.sender).get._1
          val r_net:BigInt = ls.get(tx.receiver).get._1
          val r_txC:Int = ls.get(tx.receiver).get._3
          val s_new: BigInt = s_net + tx.delta - fee
          val r_new: BigInt = r_net - tx.delta + fee
          ls -= tx.sender
          ls -= tx.receiver
          ls += (tx.sender -> (s_new,true,tx.nonce-1))
          ls += (tx.receiver -> (r_new,true,r_txC))
        } else if (tx.receiver == forger) {
          val s_net:BigInt = ls.get(tx.sender).get._1
          val r_net:BigInt = ls.get(tx.receiver).get._1
          val r_txC:Int = ls.get(tx.receiver).get._3
          val s_new: BigInt = s_net + tx.delta
          val r_new: BigInt = r_net - tx.delta
          ls -= tx.sender
          ls -= tx.receiver
          ls += (tx.sender -> (s_new,true,tx.nonce-1))
          ls += (tx.receiver -> (r_new,true,r_txC))
        } else {
          val s_net:BigInt = ls.get(tx.sender).get._1
          val r_net:BigInt = ls.get(tx.receiver).get._1
          val r_txC:Int = ls.get(tx.receiver).get._3
          val f_net:BigInt = ls.get(forger).get._1
          val f_txC:Int = ls.get(forger).get._3
          val s_new: BigInt = s_net + tx.delta
          val r_new: BigInt = r_net - tx.delta + fee
          val f_new: BigInt = f_net - fee
          ls -= tx.sender
          ls -= tx.receiver
          ls -= forger
          ls += (tx.sender -> (s_new,true,tx.nonce-1))
          ls += (tx.receiver -> (r_new,true,r_txC))
          ls += (forger -> (f_new,true,f_txC))
        }
    } else {
      if (tx.sender == forger) {
        val s_net:BigInt = ls.get(tx.sender).get._1
        val r_net:BigInt = ls.get(tx.receiver).get._1
        val s_new: BigInt = s_net + tx.delta - fee
        val r_new: BigInt = r_net - tx.delta + fee
        ls -= tx.sender
        ls -= tx.receiver
        assert(r_new == 0)
        ls += (tx.sender -> (s_new,true,tx.nonce-1))
      } else {
        val s_net:BigInt = ls.get(tx.sender).get._1
        val r_net:BigInt = ls.get(tx.receiver).get._1
        val f_net:BigInt = ls.get(forger).get._1
        val f_txC = ls.get(forger).get._3
        val s_new: BigInt = s_net + tx.delta
        val r_new: BigInt = r_net - tx.delta + fee
        val f_new: BigInt = f_net - fee
        ls -= tx.sender
        ls -= forger
        assert(r_new == 0)
        ls += (tx.sender -> (s_new,true,tx.nonce-1))
        ls += (forger -> (f_new,true,f_txC))
      }
    }
    ls
  } match {
    case Success(value) => Some(value)
    case Failure(exception) =>
      exception.printStackTrace()
      None
  }

}
