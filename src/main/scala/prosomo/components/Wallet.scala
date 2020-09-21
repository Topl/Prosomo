package prosomo.components

import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives.{Fch, Sig, Types}
import prosomo.stakeholder.Transactions
import prosomo.primitives.Parameters
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.math.BigInt
import scala.util.Random

/**
  * AMS 2020:
  * Wallet for tracking transactions pertaining to the provided public address
  * @param pkw public address of the wallet
  */

case class Wallet(pkw:ByteArrayWrapper) extends Types with Transactions {
  val fee_r = Parameters.fee_r
  var pendingTxsOut:Map[Sid,Transaction] = Map()
  var availableBalance:BigInt = 0
  var totalBalance:BigInt = 0
  var txCounter:Int = 0
  var confirmedTxCounter:Int = 0
  var netStake:BigInt = 1
  var netStake0:BigInt = 1
  var issueState:State = State(mutable.Map())
  var confirmedState:State = State(mutable.Map())
  var reallocated:Map[PublicKeyW,Int] = Map()
  val fch:Fch = new Fch

  def addTx(transaction: Transaction): Unit = {
    if (transaction.sender == pkw) {
      if (!pendingTxsOut.keySet.contains(transaction.sid)) {
        pendingTxsOut += (transaction.sid -> transaction)
      }
    }
  }

  def removeTx(transaction: Transaction): Unit = {
    if (transaction.sender == pkw) {
      if (pendingTxsOut.keySet.contains(transaction.sid)) {
        pendingTxsOut -= transaction.sid
      }
    }
  }

  def getConfirmedBalance:BigInt = {
    confirmedState.get(pkw) match {
      case Some(info) =>
        availableBalance = info._1
        availableBalance
      case None => BigInt(0)
    }
  }

  def getPendingBalance:BigInt = {
    issueState.get(pkw) match {
      case Some(info) =>
        totalBalance = info._1
        totalBalance
      case None => BigInt(0)
    }
  }

  def getNumPending:Int = {
    pendingTxsOut.keySet.size
  }

  def getTxCounter:Int = {
    issueState.get(pkw) match {
      case Some(info) =>
        txCounter = info._3
        txCounter
      case None => 0
    }
  }

  def getConfirmedTxCounter:Int = {
    confirmedState.get(pkw) match {
      case Some(info) =>
        confirmedTxCounter = info._3
        confirmedTxCounter
      case None => 0
    }
  }

  def update(state:State): Unit = {
    issueState = state.copy
    confirmedState = state.copy
    for (entry <- pendingTxsOut) {
      issueState.get(pkw) match {
        case Some(value) =>
          if (entry._2.nonce < value._3) {
            removeTx(entry._2)
          }
        case None =>
      }

    }
    for (entry <- sortPendingTx) {
      val trans = entry._2
      applyTransaction(trans,issueState,None,fee_r) match {
        case Some(value:StateData) =>
          issueState = value
        case _ =>
          pendingTxsOut = Map()
      }
    }
    for (entry<-reallocated.keySet) {
      confirmedState.get(entry) match {
        case None => reallocated -= entry
        case Some(info) =>
          if (info._3 > reallocated(entry) && info._1 > 0) {
            reallocated -= entry
          } else if (info._1 == 0) {
            reallocated -= entry
          }
      }
    }
  }

  def getPending(state:State):List[Transaction] = {
    var out:List[Transaction] = List()
    for (entry <- pendingTxsOut) {
      state.get(pkw) match {
        case Some(value) =>
          if (entry._2.nonce >= value._3) {
            out ::= entry._2
          }
        case None =>
      }

    }
    out
  }

  def add(ledger:TransactionSet): Unit = {
    for (entry <- ledger) {
      entry match {
        case transaction: Transaction =>
          addTx(transaction)
        case _ =>
      }
    }
  }

  def remove(ledger:TransactionSet): Unit = {
    for (entry <- ledger) {
      entry match {
        case transaction: Transaction =>
          removeTx(transaction)
        case _ =>
      }
    }
  }

  def sortPendingTx: ListMap[Sid, Transaction] = {
    ListMap(pendingTxsOut.toSeq.sortWith(_._2.nonce < _._2.nonce): _*)
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
  def signTransaction(sk_s:PrivateKey, pk_s:PublicKeyW, pk_r:PublicKeyW, delta:BigInt, txCounter:Int,sig:Sig,rng:Random,serializer: Serializer): Transaction = {
    val sid:Sid = hash(rng.nextString(64),serializer)
    val trans:Transaction = Transaction(pk_s,pk_r,delta,sid,txCounter,sig.sign(sk_s,pk_r.data++delta.toByteArray++sid.data++serializer.getBytes(txCounter)))
    trans
  }


  def issueTx(data:(ByteArrayWrapper,BigInt),sk_sig:Array[Byte],sig:Sig,rng:Random,serializer: Serializer): Option[Transaction] = {
    issueState.get(pkw) match {
      case Some(value) =>
        val (pk_r,delta) = data
        val scaledDelta = BigDecimal(delta.toDouble*netStake.toDouble/netStake0.toDouble).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        val txC:Int = value._3
        val tx:Transaction = signTransaction(sk_sig,pkw,pk_r,scaledDelta,txC,sig,rng,serializer)
        applyTransaction(tx,issueState.copy,None,fee_r) match {
          case Some(value:StateData) =>
            issueState = value
            pendingTxsOut += (tx.sid->tx)
            Some(tx)
          case _ =>
            None
        }
      case None =>
        None
    }
  }

  def issueTx(sender:PublicKeyW, recipient:PublicKeyW, delta:BigInt, sk_sig:Array[Byte], sig:Sig, rng:Random, serializer: Serializer): Option[Transaction] = {
    issueState.get(sender) match {
      case Some(value) =>
        val txC = value._3
        val tx:Transaction = signTransaction(sk_sig,sender,recipient,delta,txC,sig,rng,serializer)
        applyTransaction(tx,issueState,None,fee_r) match {
          case Some(value:StateData) =>
            if (sender == pkw) {
              issueState = value
              pendingTxsOut += (tx.sid->tx)
            }
            Some(tx)
          case _ =>
            None
        }
      case None => None
    }
  }

  def isSameLedgerId(publicAddress:PublicKeyW):Boolean = {
    publicAddress.data.take(pk_length).deep == pkw.data.take(pk_length).deep && publicAddress != pkw
  }

  def collectStake(sk_sig:Array[Byte], sig:Sig, rng:Random, serializer: Serializer):Seq[Transaction] = {
    var out:Seq[Transaction] = Seq()
    confirmedState.data.foreach({entry=>
      if (!reallocated.keySet.contains(entry._1)) {
        if (isSameLedgerId(entry._1) && entry._2._1 > 0) {
          issueTx(entry._1,pkw,entry._2._1,sk_sig,sig,rng,serializer) match {
            case Some(tx) =>
              reallocated += (entry._1 -> tx.nonce)
              out ++= Seq(tx)
            case None =>
          }
        }
      }
    })
    out
  }

}
