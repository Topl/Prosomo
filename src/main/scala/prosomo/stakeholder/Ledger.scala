package prosomo.stakeholder

import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.SendTx
import prosomo.components.{Tine, Transaction, State}
import prosomo.primitives.SharedData
import scorex.util.encode.Base58

import scala.collection.immutable.ListMap
import scala.math.BigInt
import scala.util.{Success,Failure, Try}
import scala.util.control.Breaks.{break, breakable}

/**
  * AMS 2020:
  * Methods regarding ledger updates and state transitions using the account based transaction model,
  * Many more Tx models are to be incorporated, namely UTXOs,
  * this outlines where state updates will be executed in the system
  */

trait Ledger extends Members {

  def updateWallet():Unit = Try {
    var id = localChain.getLastActiveSlot(globalSlot).get
    val bn:Int = getBlockHeader(id).get._9
    if (bn == 0) {
      wallet.update(localState)
      if (holderIndex == SharedData.printingHolder) {
        SharedData.walletInfo = (
          wallet.getNumPending,
          wallet.getConfirmedTxCounter,
          wallet.getConfirmedBalance,
          wallet.getPendingBalance
        )
        SharedData.issueTxInfo = Some((keys.pkw,inbox))
        SharedData.selfWrapper = Some(selfWrapper)
      }
    } else {
      breakable{
        val newState = localState.copy
        unapplyBlock(newState,id) match {
          case Some(_) =>
          case None => assert(false)
        }
        while (true) {
          id = getParentId(id).get
          getBlockHeader(id) match {
            case Some(b:BlockHeader) =>
              val bni = b._9
              if (bni <= bn-confirmationDepth || bni == 0) {
                wallet.update(newState)
                if (holderIndex == SharedData.printingHolder) {
                  SharedData.walletInfo = (
                    wallet.getNumPending,
                    wallet.getConfirmedTxCounter,
                    wallet.getConfirmedBalance,
                    wallet.getPendingBalance
                  )
                  SharedData.issueTxInfo = Some((keys.pkw,inbox))
                  SharedData.selfWrapper = Some(selfWrapper)
                }
                break
              } else {
                unapplyBlock(newState,id) match {
                  case Some(_) =>
                  case None => assert(false)
                }
              }
            case None =>
              println("Error: invalid id in wallet")
              break
          }
        }
      }
    }
    for (trans:Transaction <- wallet.getPending(localState)) {
      if (!memPool.keySet.contains(trans.sid)) memPool += (trans.sid->(trans,0))
      send(selfWrapper,gossipSet(selfWrapper,holders), SendTx(trans,selfWrapper))
    }
    walletStorage.store(wallet,serializer)
  } match {
    case Failure(exception) =>
      exception.printStackTrace()
    case _ =>
  }

  def collectStake():Unit = {
    wallet.collectStake(keys.sk_sig, sig, rng, serializer).foreach({tx =>
      if (holderIndex == SharedData.printingHolder && printFlag)
        println("Holder " + holderIndex.toString + " Reallocated Stake")
      txCounter += 1
      memPool += (tx.sid->(tx,0))
      send(selfWrapper,gossipSet(selfWrapper,holders), SendTx(tx,selfWrapper))
    })
  }

  def applyBlockReward(ls:State, pk_f:PublicKeyW): Option[State] = Try{
    val f_net: BigInt = ls.get(pk_f).get._1
    val txC:Int = ls.get(pk_f).get._3
    val f_new: BigInt = f_net + forgerReward
    ls -= pk_f
    ls += (pk_f -> (f_new,true,txC))
    ls
  } match {
    case Success(value) => Some(value)
    case Failure(exception) =>
      exception.printStackTrace()
      None
  }

  def unapplyBlockReward(ls:State, pk_f:PublicKeyW): Option[State] = Try{
    val f_net: BigInt = ls.get(pk_f).get._1
    val txC:Int = ls.get(pk_f).get._3
    val f_new: BigInt = f_net - forgerReward
    ls -= pk_f
    ls += (pk_f -> (f_new,true,txC))
    ls
  } match {
    case Success(value) => Some(value)
    case Failure(exception) =>
      exception.printStackTrace()
      None
  }

  def applyGenesisSet(ls:State, genesisSet: GenesisSet): Option[State] = Try{
    assert(genesisSet.nonEmpty)
    for (entry <- genesisSet) {
      if (ByteArrayWrapper(entry._1) == genesisBytes) {
        val delta = entry._3
        val netStake:BigInt = 0
        val newStake:BigInt = netStake + delta
        val pk_g:PublicKeyW = entry._2
        ls.get(pk_g) match {
          case None =>
            ls += (pk_g -> (newStake,true,0))
          case _ => assert(false)
        }
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
    * apply each block in tine to local state in order
    * @param ls old local state to be updated
    * @param c chain of block ids
    * @return updated localstate
    */

  def applyTine(ls:State, c:Tine): Option[State] = Try{
    for (id <- c.ordered) {
      applyBlock(ls,id) match {
        case None => assert(false)
        case _ =>
      }
    }
    ls
  } match {
    case Success(value) => Some(value)
    case Failure(exception) =>
      exception.printStackTrace()
      None
  }

  def unapplyTine(ls:State, c:Tine): Option[State] = Try{
    for (id <- c.ordered.reverse) {
      unapplyBlock(ls,id) match {
        case None => assert(false)
        case _ =>
      }
    }
    ls
  } match {
    case Success(value) => Some(value)
    case Failure(exception) =>
      exception.printStackTrace()
      None
  }

  def applyBlock(ls:State, id:SlotId):Option[State] = Try{
    getBlockHeader(id) match {
      case Some(b) =>
        val (_,_,slot:Slot,_,_,_,_,pk_kes:PublicKey,_,_) = b
        val cert:Cert = b._4
        val (pk_vrf,_,_,pk_sig,_,_) = cert
        val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
        if (slot == 0) {
          val genesisSet:GenesisSet = blocks.get(id).get.genesisSet.get
          applyGenesisSet(ls,genesisSet) match {
            case Some(_) =>
            case None => assert(false)
          }
        } else {
          //apply forger reward
          applyBlockReward(ls,pk_f)
          //apply transactions
          for (tx <- blocks.get(id).get.blockBody.get) {
            assert(verifyTransaction(tx))
            applyTransaction(tx, ls, pk_f, fee_r) match {
              case Some(_) =>
              case None => assert(false)
            }
          }
        }
      case None =>
        assert(false)
    }
    ls
  } match {
    case Success(value) => Some(value)
    case Failure(exception) =>
      println(s"Holder $holderIndex ledger error on slot "+id._1+" block id:"+Base58.encode(id._2.data))
      SharedData.throwError(holderIndex)
      exception.printStackTrace()
      None
  }

  def unapplyBlock(ls:State, id:SlotId):Option[State] = {
    getBlockHeader(id) match {
      case Some(b) => Try{
        val (_,_,slot:Slot,_,_,_,_,pk_kes:PublicKey,_,_) = b
        val cert:Cert = b._4
        val (pk_vrf,_,_,pk_sig,_,_) = cert
        val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
        var validForger = true

        //unapply transactions
        for (trans <- blocks.get(id).get.blockBody.get.reverse) {
          unapplyTransaction(trans, ls, pk_f, fee_r) match {
            case Some(_) =>
            case None => assert(false)
          }
        }
        //unapply forger reward
        unapplyBlockReward(ls,pk_f) match {
          case Some(_) =>
          case None => assert(false)
        }
        ls
      }.toOption
      case None => None
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
      if (entry._2._1.nonce < localState.get(entry._2._1.sender).get._3) {
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

  def blockify(pkw:PublicKeyW, mp:MemPool, ls:State): TransactionSet = {
    var newBlockBody: Seq[Transaction] = Seq()
    val sortedBuffer = ListMap(mp.toSeq.sortWith(_._2._1.nonce < _._2._1.nonce): _*)
    breakable {
      for (entry <- sortedBuffer) {
        val tx:Transaction = entry._2._1
        val txCount:Int = tx.nonce
        if (txCount == ls.get(tx.sender).get._3 && verifyTransaction(tx)) {
          applyTransaction(tx,ls, pkw,fee_r) match {
            case Some(_) =>
              newBlockBody ++= Seq(entry._2._1)
            case _ =>
          }
          if (newBlockBody.length >= txPerBlock) break
        }
      }
    }
    newBlockBody
  }

}
