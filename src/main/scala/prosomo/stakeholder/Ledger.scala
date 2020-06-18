package prosomo.stakeholder

import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Tine, Transaction}
import prosomo.primitives.{Parameters, SharedData}
import scorex.util.encode.Base58
import scala.collection.immutable.ListMap
import scala.math.BigInt
import scala.util.control.Breaks.{break, breakable}

/**
  * AMS 2020:
  * Methods regarding leger updates and state transitions using the account based transaction model,
  * Many more Tx models are to be incorporated, namely UTXOs, this outlines where state updates will be executed in the system
  */

trait Ledger extends Members {
  import Parameters._
  /**
    * apply each block in chain to passed local state
    * @param ls old local state to be updated
    * @param c chain of block ids
    * @return updated localstate
    */
  def updateLocalState(ls:State, c:Tine): Option[State] = {
    var nls:State = ls
    var isValid = true
    for (id <- c.ordered) {
      if (isValid) getBlockHeader(id) match {
        case Some(b:BlockHeader) =>
          val (_,_,slot:Slot,_,_,_,_,pk_kes:PublicKey,_,_) = b
          val cert:Cert = b._4
          val (pk_vrf,_,_,pk_sig,_,_) = cert
          val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
          var validForger = true
          if (slot == 0) {
            val genesisSet:GenesisSet = blocks.get(id).get.genesisSet.get
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
              for (trans <- blocks.get(id).get.blockBody.get) {
                if (verifyTransaction(trans)) {
                  applyTransaction(trans, nls, pk_f, fee_r) match {
                    case Some(value: State) =>
                      nls = value
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
        case _ =>
      }
      if (!isValid) {
        println(s"Holder $holderIndex ledger error on slot "+id._1+" block id:"+Base58.encode(id._2.data))
        SharedData.throwError(holderIndex)
      }
    }
    if (isValid) {
      Some(nls)
    } else {
      None
    }
  }

  def updateLocalState(ls:State, id:SlotId):Option[State] = {
    var nls:State = ls
    var isValid = true
    if (isValid) getBlockHeader(id) match {
      case Some(b:BlockHeader) =>
        val (_,_,slot:Slot,_,_,_,_,pk_kes:PublicKey,_,_) = b
        val cert:Cert = b._4
        val (pk_vrf,_,_,pk_sig,_,_) = cert
        val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
        var validForger = true
        if (slot == 0) {
          val genesisSet:GenesisSet = blocks.get(id).get.genesisSet.get
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
            for (trans <- blocks.get(id).get.blockBody.get) {
              if (verifyTransaction(trans)) {
                applyTransaction(trans, nls, pk_f, fee_r) match {
                  case Some(value:State) =>
                    nls = value
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
      case _ =>
    }
    if (!isValid) {
      println(s"Holder $holderIndex ledger error on slot "+id._1+" block id:"+Base58.encode(id._2.data))
      SharedData.throwError(holderIndex)
    }
    if (isValid) {
      Some(nls)
    } else {
      None
    }
  }

  def trimMemPool(): Unit = {
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
  def collectLedger(c:Tine): Unit = {
    for (id <- c.ordered) {
      for (trans <- blocks.get(id).get.blockBody.get) {
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
            case Some(value:State) =>
              ledger ::= entry._2._1
              ls = value
            case _ =>
          }
          if (ledger.length >= txPerBlock) break
        }
      }
    }
    ledger.reverse
  }

}
