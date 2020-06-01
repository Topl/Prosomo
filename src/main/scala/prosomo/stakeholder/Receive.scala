package prosomo.stakeholder

import java.io.BufferedWriter

import akka.actor.Cancellable
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import prosomo.primitives.{FastCryptographicHash, Kes, KeyFile, Parameters, Ratio, SharedData, Sig, Vrf}
import io.iohk.iodb.ByteArrayWrapper

import scala.concurrent.duration._
import prosomo.cases._
import prosomo.components.{Block, Serializer, Tine, Transaction}
import scorex.util.encode.Base58

import scala.math.BigInt
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}
import scala.util.{Failure, Success, Try}

trait Receive extends Members {
  import Parameters._
  def receive: Receive = {

    /**************************************************** Network Messages *********************************************************/

    /**updates time, the kes key, and resets variables
      * main runtime loop */
    case Update => {
      update
      context.system.scheduler.scheduleOnce(updateTime,self,Update)(context.system.dispatcher,self)
    }

    /**adds confirmed transactions to buffer and sends new ones to gossipers*/
    case value:SendTx => {
      if (!actorStalled) {
        if (!memPool.keySet.contains(value.transaction.sid) && localState.keySet.contains(value.transaction.sender)) {
          if (localState(value.transaction.sender)._3 <= value.transaction.nonce) {
            if (verifyTransaction(value.transaction)) {
              memPool += (value.transaction.sid->(value.transaction,0))
              send(selfWrapper,gossipers, SendTx(value.transaction))
            }
          }
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }
    }

    /**block passing, new blocks delivered are added to list of tines and then sent to gossipers*/
    case value:SendBlock => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.mac.sid)) {
          val foundBlock = blocks.knownIfPresent((value.block.slot,value.block.id))
          if (!foundBlock) {
            val b:BlockHeader = value.block.prosomoHeader
            val bHash = hash(b,serializer)
            val bSlot = b._3
            val bRho = b._5
            if (verifyMac(value.block.id,value.mac)) {
              if (verifyBlock(value.block)) {
                blocks.add(value.block)
                if (bSlot <= globalSlot) {
                  //if (holderIndex == SharedData.printingHolder && printFlag) {println("Holder " + holderIndex.toString + " Got New Tine")}
                  val newId = (bSlot, bHash)
                  send(selfWrapper,gossipers, SendBlock(value.block,signMac(value.block.id, sessionId, keys.sk_sig, keys.pk_sig)))
                  if (!bootStrapLock) {
                    if (tines.keySet.size < tineMaxTries) {
                      val jobNumber = tineCounter
                      tines += (jobNumber -> (Tine(newId,bRho),0,0,0,inbox(value.mac.sid)._1))
                      buildTine((jobNumber,tines(jobNumber)))
                      tineCounter += 1
                    } else {
                      tines.foreach(job => buildTine(job))
                    }
                  }
                }
              } else {println("error: invalid block")}
            } else {println("error: invalid block info")}
          }
        }
      }
      if (useFencing) {
        chainUpdateLock = true
        while (chainUpdateLock) {
          update
        }
        routerRef ! Flag(selfWrapper,"passData")
      }
    }

    case BootstrapJob => {
      println(s"Holder $holderIndex Bootstrapping...")
      buildTine((bootStrapJob,tines(bootStrapJob)))
    }

    /**block passing, returned blocks are added to block database*/
    case value:ReturnBlocks => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.mac.sid)) {
          def blockToId(b:Block):SlotId = (b.slot,b.id)
          if (verifyMac(hash((value.blocks.map(block => blockToId(block)),0,value.job),serializer),value.mac)) {
            for (block <- value.blocks) {
              if (!blocks.knownIfPresent(blockToId(block))) {
                if (verifyBlock(block)) {
                  if (holderIndex == SharedData.printingHolder && printFlag) {
                    println("Holder " + holderIndex.toString + " Got Block "+Base58.encode(block.id.data))
                  }
                  blocks.add(block)
                } else {println("error: invalid returned block")}
                if (tines.keySet.contains(value.job)) {
                  if (bootStrapLock) {
                    if (value.job == bootStrapJob) {
                      bootStrapMessage match {
                        case scheduledMessage:Cancellable => scheduledMessage.cancel
                        case null =>
                      }
                      bootStrapMessage = context.system.scheduler.scheduleOnce(5*slotT.millis,self,BootstrapJob)(context.system.dispatcher,self)
                    }
                  } else {
                    buildTine((value.job,tines(value.job)))
                  }
                }
              }
            }
          } else {println("error: invalid block list")}
        }
      }
      if (useFencing) {
        chainUpdateLock = true
        while (chainUpdateLock) {
          update
        }
        routerRef ! Flag(selfWrapper,"passData")
      }
    }

    /**block passing, parent ids that are not found are requested*/
    case value:RequestBlock => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.mac.sid)) {
          if (holderIndex == SharedData.printingHolder && printFlag) {
            println("Holder " + holderIndex.toString + " Was Requested Block")
          }
          if (verifyMac(hash((List(value.id),0,value.job),serializer),value.mac)) {
            val ref = inbox(value.mac.sid)._1
            blocks.getIfPresent(value.id) match {
              case Some(returnedBlock:Block) => {
                send(selfWrapper,ref,ReturnBlocks(List(returnedBlock),signMac(hash((List(value.id),0,value.job),serializer),sessionId,keys.sk_sig,keys.pk_sig),value.job))
                if (holderIndex == SharedData.printingHolder && printFlag) {
                  println("Holder " + holderIndex.toString + " Returned Block")
                }
              }
              case None =>
            }
          } else {println("error: request block invalid mac")}
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }
    }

    /**block passing, parent ids are requested with increasing depth of chain up to a finite number of attempts*/
    case value:RequestTine => {
      if (!actorStalled) {
        tineProvider match {
          case None => {
            if (inbox.keySet.contains(value.mac.sid)) {
              val request:Request = (List(value.id),value.depth,value.job)
              if (verifyMac(hash(request,serializer),value.mac) && value.depth <= tineMaxDepth) {
                val refToSend = inbox(value.mac.sid)._1
                val startId:SlotId = value.id
                val depth:Int = value.depth
                tineProvider = Try{ActorRefWrapper(context.actorOf(RequestTineProvider.props(blocks), "TineProvider"))}.toOption
                tineProvider match {
                  case Some(ref:ActorRefWrapper) =>
                    ref ! RequestTineProvider.Info(refToSend,startId,depth,selfWrapper,holderIndex,sessionId,keys.sk_sig,keys.pk_sig,value.job)
                  case None => println("error: tine provider not initialized")
                }
              } else {println("error: chain request mac invalid")}
            } else {println("error: invalid sid")}
          }
          case _ =>
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }
    }

    case RequestTineProvider.Done => {
      tineProvider = None
    }

    /**gossip protocol greeting message for populating inbox*/
    case value:Hello => {
      if (!actorStalled) {
        if (gossipers.length < numGossipers + gOff) {
          if (verifyMac(hash(value.ref,serializer),value.mac)) {
            if (!gossipers.contains(value.ref) && inbox.keySet.contains(value.mac.sid)) {
              //if (holderIndex == SharedData.printingHolder && printFlag) {println("Holder " + holderIndex.toString + " Adding Gossiper")}
              if (inbox(value.mac.sid)._1 == value.ref) gossipers = gossipers ++ List(value.ref)
              send(selfWrapper,value.ref,Hello(selfWrapper,signMac(hash(selfWrapper,serializer), sessionId, keys.sk_sig, keys.pk_sig)))
            }
          }
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }
    }

    /************************************************** From Coordinator *******************************************************/

    /**issue a transaction generated by the coordinator and send it to the list of gossipers*/
    case value:IssueTx => {
      if (!actorStalled) {
        inbox.toSeq.find(_._2._1 == value.ref) match {
          case Some(data:(Sid,(ActorRefWrapper,PublicKeys))) => {
            val pks = data._2._2
            val pkw = ByteArrayWrapper(pks._1 ++ pks._2 ++ pks._3)
            wallet.issueTx((pkw,value.delta),keys.sk_sig,sig,rng,serializer) match {
              case Some(trans:Transaction) => {
                walletStorage.store(wallet,serializer)
                txCounter += 1
                memPool += (trans.sid->(trans,0))
                send(selfWrapper,gossipers, SendTx(trans))
              }
              case _ =>
            }
          }
          case None =>
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }
    }

    case value:IssueTxToAddress => {
      if (!actorStalled) {
        wallet.issueTx((value.recip,value.delta),keys.sk_sig,sig,rng,serializer) match {
          case Some(trans:Transaction) => {
            walletStorage.store(wallet,serializer)
            txCounter += 1
            memPool += (trans.sid->(trans,0))
            send(selfWrapper,gossipers, SendTx(trans))
          }
          case _ =>
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }
    }



    /**sends holder information for populating inbox*/
    case Diffuse => {
      holders.filterNot(_ == selfWrapper).foreach(
        _ ! DiffuseData(selfWrapper,keys.publicKeys,signMac(hash((selfWrapper,keys.publicKeys),serializer), sessionId, keys.sk_sig, keys.pk_sig))
      )
      context.system.scheduler.scheduleOnce(10*slotT.millis,self,Diffuse)(context.system.dispatcher,self)
    }

    /**validates diffused string from other holders and stores in inbox */
    case value:DiffuseData => {
      if (verifyMac(hash((value.ref,value.pks),serializer),value.mac) && !inbox.keySet.contains(value.mac.sid)) {
        inbox += (value.mac.sid->(value.ref,value.pks))
        value.ref ! DiffuseData(selfWrapper,keys.publicKeys,signMac(hash((selfWrapper,keys.publicKeys),serializer), sessionId, keys.sk_sig, keys.pk_sig))
      }
    }

    /**allocation and vars of simulation*/
    case Initialize(gs) => {
      globalSlot = gs
      println("Holder "+holderIndex.toString+s" starting on global slot ${globalSlot}")
      password = s"password_holder_$holderIndex"
      salt = FastCryptographicHash(uuid)
      derivedKey = KeyFile.getDerivedKey(password,salt)
      def generateNewKeys:Unit = {
        println("Generating new keyfile...")
        val rngSeed:Random = new Random
        rngSeed.setSeed(BigInt(seed).toLong)
        val seed1 = FastCryptographicHash(rngSeed.nextString(32))
        val seed2 = FastCryptographicHash(rngSeed.nextString(32))
        val seed3 = FastCryptographicHash(rngSeed.nextString(32))
        keyFile = KeyFile.fromSeed(
          password,
          storageDir,
          serializer,
          sig:Sig,
          vrf:Vrf,
          kes:Kes,
          globalSlot,
          seed1,
          seed2,
          seed3
        )
      }
      Try{KeyFile.restore(storageDir)} match {
        case Success(Some(restoredFile:KeyFile)) => {
          println("Reading keyfile ...")
          keyFile = restoredFile
        }
        case Success(None) => {
          generateNewKeys
        }
        case Failure(exception) => {
          exception.printStackTrace()
          generateNewKeys
        }
      }
      keys = keyFile.getKeys(password,serializer,sig,vrf,kes)
      wallet = walletStorage.restore(serializer,keys.pkw,fee_r)
      val genesisBlock = blocks.getIfPresent((0,genBlockHash))
      chainStorage.restore(localChainId,serializer) match {
        case newChain:Tine if newChain.isEmpty => {
          localChain.update((0,genBlockHash),genesisBlock.get.blockHeader.get._5)
          //chainHistory.update((0,genBlockHash),serializer)
          updateLocalState(localState, (0,genBlockHash)) match {
            case Some(value:State) => localState = value
            case _ => {
              SharedData.throwError(holderIndex)
              println("error: invalid genesis block")
            }
          }
          eta = eta_from_genesis(localChain, 0)
          println("Adding genesis state to history")
          history.add((0,genBlockHash),localState,eta)
          stakingState = getStakingState(currentEpoch,localChain)
          alphaCache match {
            case Some(loadingCache:LoadingCache[ByteArrayWrapper,Ratio]) => {
              loadingCache.invalidateAll()
            }
            case None => alphaCache = Some(
              CacheBuilder.newBuilder().build[ByteArrayWrapper,Ratio](
                new CacheLoader[ByteArrayWrapper,Ratio] {
                  def load(id:ByteArrayWrapper):Ratio = {relativeStake(id,stakingState)}
                }
              )
            )
          }
          updateWallet
        }
        case newChain:Tine if !newChain.isEmpty => {
          localChain.copy(newChain)
          val lastId = localChain.last
          localSlot = localChain.last._1
          currentEpoch = localSlot/epochLength
          val loadState = history.get(lastId).get
          localState = loadState._1
          eta = loadState._2
          stakingState = getStakingState(currentEpoch,localChain)
          alphaCache match {
            case Some(loadingCache:LoadingCache[ByteArrayWrapper,Ratio]) => {
              loadingCache.invalidateAll()
            }
            case None => alphaCache = Some(
              CacheBuilder.newBuilder().build[ByteArrayWrapper,Ratio](
                new CacheLoader[ByteArrayWrapper,Ratio] {
                  def load(id:ByteArrayWrapper):Ratio = {relativeStake(id,stakingState)}
                }
              )
            )
          }
        }
      }
      keys.alpha = alphaCache.get.get(keys.pkw)
      assert(genBlockHash == hash(genesisBlock.get.blockHeader.get,serializer))
      println("Valid Genesis Block")
      sender() ! "done"
    }

    /**starts the timer that repeats the update command*/
    case Run => {
      if (holderIndex == SharedData.printingHolder && useGui) Try{
        val win = SharedData.prosomoWindow.get
        win.pendingTxField.get.enabled = true
        win.issueTxButton.get.enabled = true
      }
      if (!useFencing) {
        context.system.scheduler.scheduleOnce(updateTime,self,Update)(context.system.dispatcher,self)
        timers.startPeriodicTimer(TimerKey, GetTime, updateTime)
        context.system.scheduler.scheduleOnce(slotT*((refreshInterval * rng.nextDouble).toInt).millis,self,Refresh)(context.system.dispatcher,self)
        context.system.scheduler.scheduleOnce(5*slotT.millis,self,Diffuse)(context.system.dispatcher,self)
      }
    }

    case Refresh => {
      blocks.refresh
      chainStorage.refresh
      history.refresh
      walletStorage.refresh
      context.system.scheduler.scheduleOnce(slotT*refreshInterval.millis,self,Refresh)(context.system.dispatcher,self)
    }

    case GetTime => {
      coordinatorRef ! GetTime
    }

    /**sets the initial time*/
    case value:SetClock => {
      t0 = value.t0
      sender() ! "done"
    }

    /**sets the slot from coordinator time*/
    case value:GetTime => if (!actorStalled) {
      globalSlot = ((value.t1 - t0) / slotT).toInt
    }

    /**accepts list of other holders from coordinator */
    case HoldersFromLocal(list:List[ActorRefWrapper]) => {
      holders = list
      if (useGossipProtocol) {
        gossipers = List()
        numHello = 0
      } else {
        gossipers = gossipSet(holderId,holders)
      }
      sender() ! "done"
    }

    case NewGossipers => gossipers = gossipSet(holderId,holders)

    /**accepts genesis block from coordinator */
    case gb:GenBlock => {
      genBlockHash = hash(gb.b.prosomoHeader,serializer)
      println("Holder "+holderIndex.toString+" got genesis block "+Base58.encode(genBlockHash.data))
      assert(genBlockHash == gb.b.id)
      assert(verifyBlock(gb.b))
      if (!blocks.knownIfPresent((0,gb.b.id))){
        blocks.add(gb.b)
      }
      sender() ! "done"
    }

    /**accepts coordinator ref*/
    case CoordRef(ref) => {
      coordinatorRef = ref
      sender() ! "done"
    }

    /**sets new list of holders resets gossipers*/
    case Party(list,clear) => {
      holders = list
      if (useGossipProtocol) {
        gossipers = List()
        numHello = 0
      } else {
        gossipers = gossipSet(holderId,holders)
      }
      if (clear) inbox = Map()
      sender() ! "done"
    }

    /************************************************** Tests ***********************************************************/

    case RequestGossipers => {
      sender() ! GetGossipers(gossipers)
    }

    case RequestState => {
      sender() ! GetState(stakingState)
    }

    case RequestBlockTree => {
      sender() ! GetBlockTree(blocks,0)
    }


    /**when stalled actor will do nothing when messages are received*/
    case StallActor => {
      if (!actorStalled) {actorStalled = true}
      else {actorStalled = false}
      sender() ! "done"
    }

    /**prints inbox */
    case Inbox => {
      var i = 0
      println("Holder "+holderIndex.toString+" sid:"+Base58.encode(sessionId.data)+", Inbox:")
      for (entry <- inbox) {
        println(i.toString+" "+Base58.encode(entry._1.data))
        i+=1
      }
      println("Known holders:")
      holders.foreach(r=>println(r.actorPath.toString))
      println("Gossipers:")
      gossipers.foreach(r=>println(r.actorPath.toString))
      sender() ! "done"
    }

    case GetBalance => {
      val netAvailable = wallet.getConfirmedBalance
      val netTotal = wallet.getPendingBalance
      println(s"Holder $holderIndex available balance: $netAvailable , total balance: $netTotal")
    }

    /**prints stats */
    case Verify => {
      val trueChain = verifyChain(localChain, genBlockHash)
      println("Holder "+holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toDoubleString + ", blocks forged = "
        + blocksForged.toString + "\nChain length = " + getActiveSlots(localChain).toString + ", Valid chain = "
        + trueChain.toString)
      var chainBytes:Array[Byte] = Array()
      for (id <- subChain(localChain,0,localSlot-confirmationDepth).ordered) {
        getBlockHeader(id) match {
          case Some(b:BlockHeader) => chainBytes ++= FastCryptographicHash(serializer.getBytes(b))
          case _ =>
        }
      }
      println("Public Key: "+Base58.encode(keys.pk_sig++keys.pk_vrf++keys.pk_kes))
      println("Path: "+self.path)
      println("Chain hash: " + Base58.encode(FastCryptographicHash(chainBytes))+"\n")
      if (SharedData.error){
        for (id <- localChain.ordered) {
          if (id._1 > -1) println("H:" + holderIndex.toString + "S:" + id._1.toString + "ID:" + Base58.encode(id._2.data))
        }
        println("e:" + Base58.encode(eta_from_genesis(localChain, currentEpoch)) + "\n")
      }
      sender() ! "done"
    }

    /**prints stats */
    case Status => {
      println("Holder "+holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toDoubleString + ", blocks forged = "
        + blocksForged.toString + "\nChain length = " + getActiveSlots(localChain).toString+", MemPool Size = "+memPool.size+" Num Gossipers = "+gossipers.length.toString)
      var chainBytes:Array[Byte] = Array()
      for (id <- subChain(localChain,0,localSlot-confirmationDepth).ordered) {
        getBlockHeader(id) match {
          case Some(b:BlockHeader) => {
            chainBytes ++= FastCryptographicHash(serializer.getBytes(b))
          }
          case _ =>
        }
      }
      SharedData.txCounter += txCounter
      var txCount = 0
      var allTx:List[Sid] = List()
      var duplicatesFound = false
      var allTxSlots:List[Slot] = List()
      var holderTxOnChain:List[(Sid,Transaction)] = List()
      for (id <- subChain(localChain,1,localSlot).ordered) {
        for (trans<-blocks.getIfPresent(id).get.blockBody.get) {
          if (!allTx.contains(trans.sid)) {
            if (trans.sender == keys.pkw) holderTxOnChain ::= (trans.sid,trans)
            allTx ::= trans.sid
            allTxSlots ::= id._1
            txCount+=1
          } else {
            duplicatesFound = true
            val dupIndex = allTx.indexOf(trans.sid)
          }
        }
      }
      val holderTxCount = holderTxOnChain.length
      val txCountChain = if (holderTxOnChain.isEmpty) {0} else {holderTxOnChain.head._2.nonce}
      val txCountState = math.max(localState(keys.pkw)._3-1,0)
      println(s"Tx Counts in state and chain: $txCountState, $txCountChain")
      println(s"Transactions on chain: $holderTxCount/$txCount Duplicates: $duplicatesFound")
      println("Chain hash: " + Base58.encode(FastCryptographicHash(chainBytes))+"\n")
      sender() ! "done"
    }

    /**writes data point to file*/
    case value:WriteFile => if (!actorStalled) {
      value.fw match {
        case fileWriter: BufferedWriter => {
          val fileString = (
            holderIndex.toString + " "
              + globalSlot.toString + " "
              + keys.alpha.toDoubleString + " "
              + blocksForged.toString + " "
              + getActiveSlots(localChain).toString + " "
              + "\n"
            )
          fileWriter.write(fileString)
        }
        case _ => println("error: data file writer not initialized")
      }
    }


    case value:GetSlot => {
      if (!actorStalled) {
        if (roundBlock == 0) globalSlot += 1
        assert(globalSlot == value.s)
        while (roundBlock == 0) {
          update
        }
      } else {
        if (useFencing) {routerRef ! Flag(selfWrapper,"updateSlot")}
      }
      sender() ! "done"
    }

    case "endStep" => if (useFencing) {
      roundBlock = 0
      routerRef ! Flag(selfWrapper,"endStep")
    }

    case "passData" => if (useFencing) {
      routerRef ! Flag(selfWrapper,"passData")
    }

    case value:Adversary => {
      value.s match {
        case "" => {
          if (adversary) {
            adversary=false
          } else {
            adversary=true
          }
        }
        case "covert" => {
          if (covert) {
            adversary=false
            covert=false
          } else {
            adversary=true
            covert=true
          }
        }
        case "nas" => {
          if (forgeAll) {
            adversary = false
            forgeAll = false
          } else {
            adversary = true
            forgeAll = true
          }
        }
        case _ => "error: Adversary command unknown"
      }
      sender() ! "done"
    }


    case unknown:Any => if (!actorStalled) {
      print("received unknown message ")
      if (sender() == coordinatorRef) {
        print("from coordinator")
      }
      if (sender() == routerRef) {
        print("from router")
      }
      if (holders.contains(sender())) {
        print("from holder "+holders.indexOf(sender()).toString)
      }
      println(": "+unknown.getClass.toString)
    }
  }
}
