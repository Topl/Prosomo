package prosomo.wallet

import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives.{Ratio, Sig, Types}
import prosomo.components.{Serializer, Transaction}
import prosomo.stakeholder.Transactions

import scala.collection.immutable.ListMap
import scala.math.BigInt
import scala.util.Random

case class Wallet(pkw:ByteArrayWrapper,fee_r:Ratio) extends Types with Transactions {
  var pendingTxsOut:Map[Sid,Transaction] = Map()
  var availableBalance:BigInt = 0
  var totalBalance:BigInt = 0
  var txCounter:Int = 0
  var confirmedTxCounter:Int = 0
  var netStake:BigInt = 1
  var netStake0:BigInt = 1
  var issueState:State = Map()
  var confirmedState:State = Map()

  def addTx(transaction: Transaction) = {
    if (transaction.sender == pkw) {
      if (!pendingTxsOut.keySet.contains(transaction.sid)) {
        pendingTxsOut += (transaction.sid -> transaction)
      }
    }
  }

  def removeTx(transaction: Transaction) = {
    if (transaction.sender == pkw) {
      if (pendingTxsOut.keySet.contains(transaction.sid)) {
        pendingTxsOut -= transaction.sid
      }
    }
  }

  def getBalance:BigInt = {
    availableBalance = confirmedState(pkw)._1
    availableBalance
  }

  def getTotalBalance:BigInt = {
    totalBalance = issueState(pkw)._1
    totalBalance
  }

  def getTxCounter:Int = {
    txCounter = issueState(pkw)._3
    txCounter
  }

  def getConfirmedTxCounter:Int = {
    confirmedTxCounter = confirmedState(pkw)._3
    confirmedTxCounter
  }

  def update(state:State) = {
    issueState = state
    confirmedState = state
    for (entry <- pendingTxsOut) {
      if (entry._2.nonce < issueState(pkw)._3) {
        removeTx(entry._2)
      }
    }
    for (entry <- sortPendingTx) {
      val trans = entry._2
      applyTransaction(trans,issueState,ByteArrayWrapper(Array()),fee_r) match {
        case value:State => {
          issueState = value
        }
        case _ => {
          println("Wallet error, clearing pending Txs")
          pendingTxsOut = Map()
        }
      }
    }
  }

  def getPending(state:State):List[Transaction] = {
    var out:List[Transaction] = List()
    for (entry <- pendingTxsOut) {
      if (entry._2.nonce >= state(pkw)._3) {
        out ::= entry._2
      }
    }
    out
  }

  def add(ledger:TransactionSet) = {
    for (entry <- ledger) {
      entry match {
        case transaction: Transaction => {
          addTx(transaction)
        }
        case _ =>
      }
    }
  }

  def remove(ledger:TransactionSet) = {
    for (entry <- ledger) {
      entry match {
        case transaction: Transaction => {
          removeTx(transaction)
        }
        case _ =>
      }
    }
  }

  def sortPendingTx = {
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
    val trans:Transaction = new Transaction(pk_s,pk_r,delta,sid,txCounter,sig.sign(sk_s,pk_r.data++delta.toByteArray++sid.data++serializer.getBytes(txCounter)))
    trans
  }


  def issueTx(data:(ByteArrayWrapper,BigInt),sk_sig:Array[Byte],sig:Sig,rng:Random,serializer: Serializer): Any = {
    if (issueState.keySet.contains(pkw)) {
      val (pk_r,delta) = data
      val scaledDelta = BigDecimal(delta.toDouble*netStake.toDouble/netStake0.toDouble).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
      val txC = issueState(pkw)._3
      val trans:Transaction = signTransaction(sk_sig,pkw,pk_r,scaledDelta,txC,sig,rng,serializer)
      applyTransaction(trans,issueState,ByteArrayWrapper(Array()),fee_r) match {
        case value:State => {
          issueState = value
          pendingTxsOut += (trans.sid->trans)
          trans
        }
        case _ => {
          0
        }
      }
    } else {
      0
    }
  }
}
