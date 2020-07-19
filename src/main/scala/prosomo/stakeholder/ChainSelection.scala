package prosomo.stakeholder

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.{BootstrapJob, RequestBlock, RequestTine, SendTx}
import prosomo.components.{Tine, Transaction}
import prosomo.primitives.{ActorRefWrapper, Ratio, SharedData}
import scorex.util.encode.Base58

import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks.{break, breakable}

/**
  * AMS 2020:
  * Primary consensus routines,
  * Implements the Maxvalid-BG chain selection rule specified in Ouroboros Genesis
  *
  * Tinepool functionality is executed in these methods,
  * when a common ancestor is not found for a block a request is made for the block id and sent to the network.
  *
  * Stateful validation and ledger accumulation is executed here,
  * Txs are filed into MemPool from every block encountered.
  */

trait ChainSelection extends Members {

  def updateWallet():Unit = Try{
    var id = localChain.getLastActiveSlot(globalSlot).get
    val bn:Int = getBlockHeader(id).get._9
    if (bn == 0) {
      wallet.update(history.get(id).get._1)
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
        while (true) {
          id = getParentId(id).get
          getBlockHeader(id) match {
            case Some(b:BlockHeader) =>
              val bni = b._9
              if (bni <= bn-confirmationDepth || bni == 0) {
                wallet.update(history.get(id).get._1)
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

    def collectStake():Unit = Try{
      for (entry<-wallet.confirmedState) if (!wallet.reallocated.keySet.contains(entry._1)) {
        if (wallet.isSameLedgerId(entry._1) && entry._2._1 > 0) {
          wallet.issueTx(entry._1,wallet.pkw,entry._2._1,keys.sk_sig,sig,rng,serializer) match {
            case Some(trans:Transaction) =>
              if (holderIndex == SharedData.printingHolder && printFlag)
                println("Holder " + holderIndex.toString + " Reallocated Stake")
              txCounter += 1
              memPool += (trans.sid->(trans,0))
              send(selfWrapper,gossipSet(selfWrapper,holders), SendTx(trans,selfWrapper))
              wallet.reallocated += (entry._1->trans.nonce)
            case _ =>
          }
        }
      }
    }
    collectStake()
    walletStorage.store(wallet,serializer)
  } match {
    case Failure(exception) =>
      exception.printStackTrace()
    case _ =>
  }

  def buildTine(job:(Int,(Tine,Int,Int,Int,ActorRefWrapper))): Unit = Try{
    val entry = job._2
    var foundAncestor = true
    val tine:Tine = entry._1
    var counter:Int = entry._2
    val previousLen:Int = entry._3
    var totalTries:Int = entry._4
    val ref:ActorRefWrapper = entry._5
    var prefix:Option[Slot] = None
    breakable{
      while(foundAncestor) {
        getParentId(tine.oldest) match {
          case Some(parentId:SlotId) =>
            if (localChain.get(parentId._1).contains(parentId)) {
              prefix = Some(parentId._1)
              assert(localChain.get(prefix.get).get == parentId)
              break
            } else {
              if (blocks.knownInCache(parentId)) {
                getBlockHeader(parentId) match {
                  case Some(pbh:BlockHeader) =>
                    tine.update(parentId,pbh._5)
                  case None =>
                    println("Error: cache error in blocks database in buildTine")
                    tinePool -= job._1
                }
              } else {
                val tineLength = tine.numActive
                if (tineLength>tineMaxDepth) {
                  getBlockHeader(parentId) match {
                    case Some(pbh:BlockHeader) =>
                      tine.update(parentId,pbh._5)
                    case None =>
                      if (job._1 >= 0 && !helloLock) {
                        bootStrapLock = true
                        if (tine.maxSlot.get - tine.minSlot.get > slotWindow) tine.loadCache()
                        bootStrapJob = job._1
                        if (holderIndex == SharedData.printingHolder && printFlag) println(
                          "Holder " + holderIndex.toString
                            + " Looking for Parent Tine, Job:"+job._1
                            +" Tries:"+counter.toString+" Length:"+tineLength+" Tines:"+tinePool.keySet.size
                        )
                        val depth:Int = if (tineLength < tineMaxDepth) {
                          tineLength
                        } else {
                          tineMaxDepth
                        }
                        send(selfWrapper,ref, RequestTine(parentId,depth,job._1,selfWrapper))
                        if (tine.numActive == previousLen) {counter+=1} else {counter=0}
                        foundAncestor = false
                      }
                  }
                } else {
                  if (holderIndex == SharedData.printingHolder && printFlag) println(
                    "Holder " + holderIndex.toString
                      + " Looking for Parent Block, Job:"+job._1
                      +" Tries:"+counter.toString+" Length:"+tine.numActive+" Tines:"+tinePool.keySet.size
                  )
                  send(selfWrapper,ref,RequestBlock(parentId,job._1,selfWrapper))
                  if (tine.numActive == previousLen) {counter+=1} else {counter=0}
                  foundAncestor = false
                }
              }
            }
          case None =>
            if (tine.numActive == previousLen) {counter+=1} else {counter=0}
            println("Error: Tine in tinepool contains oldest slotId not in database")
            foundAncestor = false
        }
      }
    }

    if (foundAncestor) {
      if (tine.notSparsePast(prefix.get)) {
        if (holderIndex == SharedData.printingHolder && printFlag)
          println(s"Tine length = ${tine.numActive} Common prefix slot = ${prefix.get}")
        tinePoolWithPrefix = Array((tine,prefix.get,job._1)) ++ tinePoolWithPrefix
      }
      tinePool -= job._1
    } else {
      totalTries += 1
      tinePool -= job._1
      if (counter<tineMaxTries) tinePool += (job._1 -> (tine,counter,tine.numActive,totalTries,ref))
    }
  } match {
    case Failure(exception) =>
      exception.printStackTrace()
      tinePool -= job._1
    case _ =>
  }

  def updateTine(inputTine:Tine): Option[(Tine,Slot)] = Try{
    val headIdOpt:Option[SlotId] = Try{inputTine.head}.toOption
    headIdOpt match {
      case Some(headId:SlotId) =>
        if (localChain.get(headId._1).contains(headId)) {
          None
        } else {
          var prefix:Option[Slot] = None
          val tine:Tine = Tine(headId,inputTine.getNonce(headId._1).get)
          @scala.annotation.tailrec
          def loop(id:SlotId):Unit = {
            getParentId(id) match {
              case Some(pid:SlotId) =>
                if (localChain.get(pid._1).contains(pid)) {
                  prefix = Some(pid._1)
                } else {
                  inputTine.getNonce(pid._1) match {
                    case Some(nonce) =>
                      tine.update(pid,nonce)
                      loop(pid)
                    case None =>
                      val nonce = blocks.get(pid).get.nonce
                      tine.update(pid,nonce)
                      loop(pid)
                  }
                }
              case None =>
                println("Error: tine update found no common prefix")
            }
          }
          loop(headId)
          prefix match {
            case Some(p) => Some((tine,p))
            case None => None
          }

        }
      case None =>
        println("Error: invalid head id in update tine")
        None
    }
  } match {
    case Failure(exception) =>
      exception.printStackTrace()
      None
    case Success(value) => value
  }

  /**
    * Chain adoption for synchronized nodes,
    * Only for nodes with a previous head close to the current global slot,
    * Ledger collection is performed on all blocks
    */

  def maxValidBG(): Unit = if (!tinePoolWithPrefix.isEmpty) {
    val job:Int = tinePoolWithPrefix.last._3
    Try{
      val prefix:Slot = tinePoolWithPrefix.last._2
      val tine:Tine = tinePoolWithPrefix.last._1
      assert(!tine.isEmpty)
      val headId = localChain.head
      val head = getBlockHeader(headId)
      val tineHeadId = tine.head
      val tineHead = getBlockHeader(tineHeadId)
      val bnt = tineHead.get._9
      val bnl = head.get._9

      if (holderIndex == SharedData.printingHolder) {
        println("Previous head: " + s" block ${head.get._9} "
          + Base58.encode(headId._2.data))
      }

      if (job == bootStrapJob && bootStrapJob >= 0) {
        routerRef ! BootstrapJob(selfWrapper)
      }

      val bestChain = if(tineHeadId._1 - prefix < k_s && bnl < bnt) {
        true
      } else {
        val slotsTine = tine.slice(prefix+1,prefix+1+slotWindow).numActive
        val slotsLocal = localChain.slice(prefix+1,prefix+1+slotWindow).numActive
        slotsLocal < slotsTine
      }

      if (bestChain) {
        if (verifyTine(tine,prefix)) {
          adoptTine()
        } else {
          println("Error: invalid best chain")
          tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
          SharedData.throwError(holderIndex)
        }
      } else {
        dropTine()
      }

      def adoptTine():Unit = {
        if (holderIndex == SharedData.printingHolder && printFlag)
          println(s"Tine Adopted  $bnt  >  $bnl")
        val reorgTine = localChain.slice(prefix+1,globalSlot)
        if (!reorgTine.isEmpty) collectLedger(reorgTine)
        collectLedger(tine)
        if (!reorgTine.isEmpty) for (id <- reorgTine.ordered) {
          val ledger:TransactionSet = blocks.get(id).get.blockBody.get
          wallet.add(ledger)
        }
        for (id <- tine.ordered) {
          val blockLedger:TransactionSet = blocks.get(id).get.blockBody.get
          for (trans<-blockLedger) {
            if (memPool.keySet.contains(trans.sid)) {
              memPool -= trans.sid
            }
          }
        }
        localChain.reorg(prefix,tine)
        val newHeadSlot = localChain.head._1
        history.get(localChain.get(newHeadSlot).get) match {
          case Some(reorgState:(State,Eta)) =>
            localState = reorgState._1
            eta = reorgState._2
          case _ =>
            println("Error: invalid state and eta on adopted tine")
            SharedData.throwError(holderIndex)
        }
        var epoch = newHeadSlot / epochLength
        for (slot <- newHeadSlot to globalSlot) {
          updateEpoch(slot,epoch,eta,localChain,None) match {
            case result:(Int,Eta) if result._1 > epoch =>
              epoch = result._1
              val stakeDistMaxSlot:Slot = epoch*epochLength-1
              val stakeDistId:SlotId = localChain.getLastActiveSlot(stakeDistMaxSlot).get
              history.cacheStakeDist(stakeDistId)
              eta = result._2
              stakingState = getStakingState(epoch,localChain,None)
              alphaCache match {
                case Some(loadingCache:LoadingCache[ByteArrayWrapper,Ratio]) =>
                  loadingCache.invalidateAll()
                case None => alphaCache = Some(
                  CacheBuilder.newBuilder().build[ByteArrayWrapper,Ratio](
                    new CacheLoader[ByteArrayWrapper,Ratio] {
                      def load(id:ByteArrayWrapper):Ratio = {relativeStake(id,stakingState)}
                    }
                  )
                )
              }
              keys.alpha = alphaCache.get.get(keys.pkw)
            case _ =>
          }
        }
        assert(currentEpoch == epoch)
        updateWallet()
        trimMemPool()
        tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
        var newCandidateTines:Array[(Tine,Slot,Int)] = Array()
        for (entry <- tinePoolWithPrefix) {
          updateTine(entry._1) match {
            case Some((newTine:Tine,prefix:Slot)) =>
              if (prefix > 0 && !newTine.isEmpty) {
                newCandidateTines = newCandidateTines ++ Array((newTine,prefix,entry._3))
              }
            case None =>
          }
        }
        tinePoolWithPrefix = newCandidateTines
      }

      def dropTine():Unit = {
        if (holderIndex == SharedData.printingHolder && printFlag)
          println(s"Tine Rejected $bnt  <= $bnl")
        collectLedger(tine)
        for (id <- localChain.slice(prefix+1,globalSlot).ordered) {
          val blockLedger:TransactionSet = blocks.get(id).get.blockBody.get
          for (trans <- blockLedger) {
            if (memPool.keySet.contains(trans.sid)){
              memPool -= trans.sid
            }
          }
        }
        tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
      }

      if (job == bootStrapJob && job >= 0) {
        bootStrapJob = -1
        bootStrapLock = false
        routerRef ! BootstrapJob(selfWrapper)
      }

      if (holderIndex == SharedData.printingHolder) {
        val newHeadId = localChain.head
        val newHead = getBlockHeader(newHeadId)
        println(Console.CYAN + "Current Slot = " + globalSlot.toString + s" on block ${newHead.get._9} "
          + Base58.encode(newHeadId._2.data) + Console.RESET)
      }
    } match {
      case Failure(exception) =>
        exception.printStackTrace()
        if (tinePoolWithPrefix.nonEmpty) {
          if (tinePoolWithPrefix.last._3 == job) tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
        }
      case _ =>
    }
  }

  /**
    * Chain adoption for bootstrapping nodes,
    * Only for nodes that are processing fetch info responses
    * No ledger collection is performed
    */
  def bootstrapAdoptTine(): Unit = if (!tinePoolWithPrefix.isEmpty) Try{
    val prefix:Slot = tinePoolWithPrefix.last._2
    val tine:Tine = tinePoolWithPrefix.last._1
    assert(!tine.isEmpty)
    val job:Int = tinePoolWithPrefix.last._3
    assert(job == -1)
    val headId = localChain.head
    val head = getBlockHeader(headId)
    val tineHeadId = tine.head
    val tineHead = getBlockHeader(tineHeadId)
    val bnt = tineHead.get._9
    val bnl = head.get._9

    if (holderIndex == SharedData.printingHolder) {
      println("Previous head: " + s" block ${head.get._9} "
        + Base58.encode(headId._2.data))
    }

    val bestChain = if(tineHeadId._1 - prefix < k_s && bnl < bnt) {
      true
    } else {
      val slotsTine = tine.slice(prefix+1,prefix+1+slotWindow).numActive
      val slotsLocal = localChain.slice(prefix+1,prefix+1+slotWindow).numActive
      slotsLocal < slotsTine
    }

    if (bestChain) {
      if (verifyTine(tine,prefix)) {
        adoptTine()
      } else {
        println("Error: invalid best chain")
        tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
        SharedData.throwError(holderIndex)
      }
    } else {
      dropTine()
    }

    def adoptTine():Unit = {
      if (holderIndex == SharedData.printingHolder && printFlag)
        println(s"Tine Adopted  $bnt  >  $bnl")
      localChain.reorg(prefix,tine)
      val newHeadSlot = localChain.head._1
      history.get(localChain.head) match {
        case Some(reorgState:(State,Eta)) =>
          localState = reorgState._1
          eta = reorgState._2
        case _ =>
          println("Error: invalid state and eta on adopted tine")
          SharedData.throwError(holderIndex)
      }
      var epoch = newHeadSlot / epochLength
      for (slot <- newHeadSlot to globalSlot) {
        updateEpoch(slot,epoch,eta,localChain,None) match {
          case result:(Int,Eta) if result._1 > epoch =>
            epoch = result._1
            val stakeDistMaxSlot:Slot = epoch*epochLength-1
            val stakeDistId:SlotId = localChain.getLastActiveSlot(stakeDistMaxSlot).get
            history.cacheStakeDist(stakeDistId)
            eta = result._2
            stakingState = getStakingState(epoch,localChain,None)
            alphaCache match {
              case Some(loadingCache:LoadingCache[ByteArrayWrapper,Ratio]) =>
                loadingCache.invalidateAll()
              case None => alphaCache = Some(
                CacheBuilder.newBuilder().build[ByteArrayWrapper,Ratio](
                  new CacheLoader[ByteArrayWrapper,Ratio] {
                    def load(id:ByteArrayWrapper):Ratio = {relativeStake(id,stakingState)}
                  }
                )
              )
            }
            keys.alpha = alphaCache.get.get(keys.pkw)
          case _ =>
        }
      }
      assert(currentEpoch == epoch)
      tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
      var newCandidateTines:Array[(Tine,Slot,Int)] = Array()
      for (entry <- tinePoolWithPrefix) {
        updateTine(entry._1) match {
          case Some((newTine:Tine,prefix:Slot)) =>
            if (prefix > 0 && !newTine.isEmpty) {
              newCandidateTines = newCandidateTines ++ Array((newTine,prefix,entry._3))
            }
          case None =>
        }
      }
      tinePoolWithPrefix = newCandidateTines
    }

    def dropTine():Unit = {
      if (holderIndex == SharedData.printingHolder && printFlag)
        println(s"Tine Rejected $bnt  <= $bnl")
      tinePoolWithPrefix = tinePoolWithPrefix.dropRight(1)
    }

    if (holderIndex == SharedData.printingHolder) {
      val newHeadId = localChain.head
      val newHead = getBlockHeader(newHeadId)
      println(Console.CYAN + "Current Slot = " + globalSlot.toString + s" on block ${newHead.get._9} "
        + Base58.encode(newHeadId._2.data) + Console.RESET)
    }
  } match {
    case Failure(exception) =>
      exception.printStackTrace()
    case _ =>
  }

}
