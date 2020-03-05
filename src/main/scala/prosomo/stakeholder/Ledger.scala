package prosomo.stakeholder

import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Chain, Transaction}
import prosomo.primitives.Parameters.fee_r
import prosomo.primitives.{Parameters, SharedData}
import scorex.crypto.encode.Base58

import scala.collection.immutable.ListMap
import scala.math.BigInt
import scala.util.control.Breaks.{break, breakable}

trait Ledger extends Members {
  import Parameters._
  /**
    * apply each block in chain to passed local state
    * @param ls old local state to be updated
    * @param c chain of block ids
    * @return updated localstate
    */
  def updateLocalState(ls:State, c:Chain): Any = {
    var nls:State = ls
    var isValid = true
    for (id <- c.ordered) {
      if (isValid) getBlockHeader(id) match {
        case b:BlockHeader => {
          val (_,_,slot:Slot,cert:Cert,_,_,_,pk_kes:PublicKey,_,_) = b
          val (pk_vrf,_,_,pk_sig,_,_) = cert
          val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
          var validForger = true
          if (slot == 0) {
            val genesisSet:GenesisSet = blocks.getGenSet(id)
            if (genesisSet.isEmpty) isValid = false
            if (isValid) for (entry <- genesisSet) {
              if (ByteArrayWrapper(entry._1) == genesisBytes && verifyMac(hashGenEntry((entry._1,entry._2,entry._3),serializer),entry._4)) {
                val delta = entry._3
                val netStake:BigInt = 0
                val newStake:BigInt = netStake + delta
                val pk_g:PublicKeyW = entry._2
                if(nls.keySet.contains(pk_g)) {
                  isValid = false
                  nls -= pk_g
                }
                nls += (pk_g -> (newStake,true,0))
              }
            }
          } else {
            //apply forger reward
            if (nls.keySet.contains(pk_f)) {
              val netStake: BigInt = nls(pk_f)._1
              val txC:Int = nls(pk_f)._3
              val newStake: BigInt = netStake + forgerReward
              nls -= pk_f
              nls += (pk_f -> (newStake,true,txC))
            } else {
              validForger = false
            }
            //apply transactions
            if (validForger) {
              for (trans <- blocks.getTxs(id)) {
                if (verifyTransaction(trans)) {
                  applyTransaction(trans, nls, pk_f, fee_r) match {
                    case value: State => {
                      nls = value
                    }
                    case _ => isValid = false
                  }
                } else {
                  isValid = false
                }
              }
            } else {
              isValid = false
            }
          }
        }
        case _ =>
      }
      if (!isValid) {
        println(s"Holder $holderIndex ledger error on slot "+id._1+" block id:"+Base58.encode(id._2.data))
        SharedData.throwError(holderIndex)
      }
    }
    if (isValid) {
      nls
    } else {
      0
    }
  }

  def trimMemPool: Unit = {
    val mp = memPool
    for (entry <- mp) {
      if (entry._2._2 < confirmationDepth) {
        val cnt = entry._2._2 + 1
        memPool -= entry._1
        memPool += (entry._1 -> (entry._2._1,cnt))
      } else {
        memPool -= entry._1
      }
      if (entry._2._1.nonce < localState(entry._2._1.sender)._3) {
        memPool -= entry._1
      }
    }
  }

  /**
    * collects all transaction on the ledger of each block in the passed chain and adds them to the buffer
    * @param c chain to collect transactions
    */
  def collectLedger(c:Chain): Unit = {
    for (id <- c.ordered) {
      for (trans <- blocks.getTxs(id)) {
        if (!memPool.keySet.contains(trans.sid)) {
          if (verifyTransaction(trans)) memPool += (trans.sid->(trans,0))
        }
      }
    }
  }

  /**
    * sorts buffer and adds transaction to ledger during block forging
    * @param pkw public key triad of forger
    * @return list of transactions
    */
  def chooseLedger(pkw:PublicKeyW,mp:MemPool,s:State): TransactionSet = {
    var ledger: List[Transaction] = List()
    var ls: State = s
    val sortedBuffer = ListMap(mp.toSeq.sortWith(_._2._1.nonce < _._2._1.nonce): _*)
    breakable {
      for (entry <- sortedBuffer) {
        val transaction:Transaction = entry._2._1
        val transactionCount:Int = transaction.nonce
        if (transactionCount == ls(transaction.sender)._3 && verifyTransaction(transaction)) {
          applyTransaction(transaction,ls, pkw,fee_r) match {
            case value:State => {
              ledger ::= entry._2._1
              ls = value
            }
            case _ =>
          }
          if (ledger.length >= txPerBlock) break
        }
      }
    }
    ledger.reverse
  }

}