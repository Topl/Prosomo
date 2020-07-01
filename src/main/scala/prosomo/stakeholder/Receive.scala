package prosomo.stakeholder

import java.io.BufferedWriter

import akka.actor.Cancellable
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import prosomo.primitives.{ Kes, KeyFile, Parameters, Ratio, SharedData, Sig, Vrf}
import io.iohk.iodb.ByteArrayWrapper

import scala.concurrent.duration._
import prosomo.cases._
import prosomo.components.{Block, Tine, Transaction}
import scorex.util.encode.Base58

import scala.math.BigInt
import scala.util.Random
import scala.util.{Failure, Success, Try}

/**
  * AMS 2020:
  * The receive statement of Stakeholder,
  * Contains cases of all messages Stakeholder will encounter from local and remote interfaces
  * Message authentication occurs for Block passing and TinePool messages
  */

trait Receive extends Members {
  import Parameters._
  def receive: Receive = {

    /** Updates time, the kes key, and resets variables
      * Primary runtime loop for consensus
      **/
    case Update =>
      update()
      context.system.scheduler.scheduleOnce(updateTime,self,Update)(context.system.dispatcher,self)

    /******************************************** Network Messages *********************************************/

    /**
      * Primary transaction passing method, each Tx signature is validated and the Tx is statefully checked
      * Newly encountered Txs are sent to gossipers
      **/
    case value:SendTx =>
      if (!actorStalled) Try{
        if (!memPool.keySet.contains(value.transaction.sid) && localState.keySet.contains(value.transaction.sender)) {
          if (localState(value.transaction.sender)._3 <= value.transaction.nonce) {
            if (verifyTransaction(value.transaction)) {
              if (!bootStrapLock) memPool += (value.transaction.sid->(value.transaction,0))
              send(selfWrapper,gossipSet(selfWrapper,value.sender,holders), SendTx(value.transaction,selfWrapper))
            }
          }
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }

    /**
      * Block passing for newly forged blocks only,
      * If block is not found in database it is added to the tinepool as a new open tine and then sent to gossipers
      * Note AMS 2020:
      * This is where tines are introduced to the tinepool,
      * blocks below the checkpoint depth and blocks from the future never enter tinepool
      * if bootstrapping no new tines are made until the job is done or the connection is lost,
      * when bootstrapping new blocks are added to database but do not enter tinepool
      **/
    case value:SendBlock =>
      if (!actorStalled) Try{
        val foundBlock = blocks.knownIfPresent((value.block.slot,value.block.id))
        if (!foundBlock) {
          val b:BlockHeader = value.block.prosomoHeader
          val bHash = hash(b,serializer)
          val bSlot = b._3
          val bRho = b._5
          if (holderIndex == SharedData.printingHolder) {
            networkDelayList ::= (t1-t0-bSlot*slotT).toDouble/slotT.toDouble
            if (networkDelayList.size > 100) networkDelayList.take(100)
            def average(points:List[Double]):Double={
              val (net,num) = points.foldLeft((0.0,0))({ case ((s,l),x)=> (x+s,1+l) })
              net/num
            }
            SharedData.averageNetworkDelay = average(networkDelayList)
          }
          if (verifyBlock(value.block)) {
            blocks.add(value.block)
            if (bSlot <= globalSlot && bSlot > globalSlot-k_s) {
              val newId = (bSlot, bHash)
              send(selfWrapper,gossipSet(selfWrapper,value.sender,holders), SendBlock(value.block,selfWrapper))
              if (!bootStrapLock) {
                if (tinePool.keySet.size > tineMaxTries) {
                  if (holderIndex == SharedData.printingHolder && printFlag)
                    println("Holder " + holderIndex.toString + " Dropping Tine")
                  tinePool -= tinePool.keySet.min
                } else {
                  for (entry <- tinePool) {
                    if (entry._2._1.last._1 <= globalSlot-k_s || !holders.contains(entry._2._5)) {
                      if (holderIndex == SharedData.printingHolder && printFlag)
                        println("Holder " + holderIndex.toString + " Dropping Tine")
                      tinePool -= entry._1
                    }
                  }
                }
                val jobNumber = tineCounter
                tinePool += (jobNumber -> (Tine(newId,bRho),0,0,0,value.sender))
                buildTine((jobNumber,tinePool(jobNumber)))
                tineCounter += 1
              }
            }
          } else {println("error: invalid block")}
        }
      }
      if (useFencing) {
        chainUpdateLock = true
        while (chainUpdateLock) {
          update()
        }
        routerRef ! Flag(selfWrapper,"passData")
      }

    /**
      * Block passing for tinepool functionality, returned blocks are added to block database
      **/
    case value:ReturnBlocks =>
      if (!actorStalled) Try{
        def blockToId(b:Block):SlotId = (b.slot,b.id)
        for (block <- value.blocks) {
          if (!blocks.knownIfPresent(blockToId(block))) {
            if (verifyBlock(block)) {
              if (holderIndex == SharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Got Block "+Base58.encode(block.id.data))
              }
              blocks.add(block)
            } else {println("Error: invalid returned block")}
          }
          if (value.job >= 0 && tinePool.keySet.contains(value.job) && !helloLock) {
            if (bootStrapLock) {
              if (value.job == bootStrapJob) {
                bootStrapMessage match {
                  case scheduledMessage:Cancellable => scheduledMessage.cancel
                  case null =>
                }
                bootStrapMessage = context.system.scheduler
                  .scheduleOnce(1*slotT.millis,self,BootstrapJob)(context.system.dispatcher,self)
              }
            } else {
              buildTine((value.job,tinePool(value.job)))
            }
          } else if (value.job == -1 && helloLock) {
            val b = block.prosomoHeader
            val bHash = hash(b,serializer)
            val bSlot = b._3
            val bRho = b._5
            if (tinePool.keySet.contains(-1)) tinePool -= -1
            tinePoolWithPrefix = Array()
            if (tinePool.keySet.isEmpty) {
              tinePool += (-1 -> (Tine((bSlot,bHash),bRho),0,0,0,value.sender))
            }
            bootStrapMessage match {
              case scheduledMessage:Cancellable => scheduledMessage.cancel
              case null =>
            }
            bootStrapMessage = context.system.scheduler
              .scheduleOnce(1*slotT.millis,self,BootstrapJob)(context.system.dispatcher,self)
          }
        }
      }
      if (useFencing) Try{
        chainUpdateLock = true
        while (chainUpdateLock) {
          update()
        }
        routerRef ! Flag(selfWrapper,"passData")
      }

    /**
      * Block requesting for tinepool functionality, parent ids that are not found are requested from peers
      **/
    case value:RequestBlock =>
      if (!actorStalled) Try{
        if (holderIndex == SharedData.printingHolder && printFlag) {
          println("Holder " + holderIndex.toString + " Was Requested Block")
        }
        blocks.getIfPresent(value.id) match {
          case Some(returnedBlock:Block) =>
            send(selfWrapper,value.sender,ReturnBlocks(List(returnedBlock),value.job,selfWrapper))
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Returned Block")
            }
          case None =>
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }

    /**
      * Block requesting for tinepool functionality, parent ids are requested with increasing
      * depth of chain up to a finite number of attempts
      * this message is sent as a result of a tine in tinepool becoming long enough to trigger bootstrapping mode,
      * Spins up a provider to search database for blocks
      **/
    case value:RequestTine =>
      if (!actorStalled) Try{
        tineProvider match {
          case None =>
            if (value.depth <= tineMaxDepth) {
              val startId:SlotId = value.id
              val depth:Int = value.depth
              tineProvider = Try{
                ActorRefWrapper(context.actorOf(RequestTineProvider.props(blocks), "TineProvider"))
              }.toOption
              tineProvider match {
                case Some(ref:ActorRefWrapper) =>
                  ref ! RequestTineProvider.Info(holderIndex,value.sender,selfWrapper,startId,depth,value.job,None)
                case None => println("error: tine provider not initialized")
              }
            } else {println("error: chain request mac invalid")}
          case _ =>
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }

    case RequestTineProvider.Done =>
      tineProvider = None

    /**
      * Greeting message for bootstrapping nodes,
      * contains the last known slot, triggers tine recovery up to current head
      **/
    case value:Hello =>
      if (!actorStalled) Try{
        tineProvider match {
          case None =>
            val startId:SlotId = localChain.getLastActiveSlot(value.slot)
            val depth:Int = tineMaxDepth
            tineProvider = Try{
              ActorRefWrapper(context.actorOf(RequestTineProvider.props(blocks), "TineProvider"))
            }.toOption
            tineProvider match {
              case Some(ref:ActorRefWrapper) =>
                ref ! RequestTineProvider.Info(
                  holderIndex,
                  value.sender,
                  selfWrapper,
                  startId,
                  depth,
                  -1,
                  Some(subChain(localChain,value.slot,globalSlot))
                )
              case None => println("Error: tine provider not initialized")
            }
          case _ =>
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }

    /**
      * Validates diffused keys from other holders and stores in inbox
      **/
    case value:DiffuseData => if (!inbox.keySet.contains(value.sid)){
        inbox += (value.sid->(value.ref,value.pks))
        send(
          selfWrapper,
          gossipSet(selfWrapper,value.sender,holders),
          DiffuseData(value.sid,value.ref,value.pks,selfWrapper)
        )
      }

    /******************************************** From Local ****************************************************/

    case BootstrapJob =>
      if (helloLock) {
        if (tinePool.keySet.isEmpty && tinePoolWithPrefix.isEmpty) {
          val lastSlot = localChain.getLastActiveSlot(globalSlot)._1
          if (globalSlot > 1 && lastSlot < globalSlot - tineMaxDepth) {
            println(s"Holder $holderIndex Bootstrapping...")
            send(
              selfWrapper,
              gossipSet(selfWrapper,holders).take(1),
              Hello(lastSlot+1, selfWrapper)
            )
            bootStrapMessage match {
              case scheduledMessage:Cancellable => scheduledMessage.cancel
              case null =>
            }
            bootStrapMessage = context.system.scheduler
              .scheduleOnce(10*slotT.millis,self,BootstrapJob)(context.system.dispatcher,self)
          } else {
            bootStrapMessage match {
              case scheduledMessage:Cancellable => scheduledMessage.cancel
              case null =>
            }
            bootStrapLock = false
            helloLock = false
          }
        } else {
          if (tinePool.keySet.contains(-1)) buildTine((-1,tinePool(-1)))
          maxValidBG()
          bootStrapMessage match {
            case scheduledMessage:Cancellable => scheduledMessage.cancel
            case null =>
          }
          bootStrapMessage = context.system.scheduler
            .scheduleOnce(10.millis,self,BootstrapJob)(context.system.dispatcher,self)
        }
      } else if (bootStrapLock && tinePool.keySet.contains(bootStrapJob)) {
        println(s"Holder $holderIndex Bootstrapping...")
        if (holders.contains(tinePool(bootStrapJob)._5)) {
          buildTine((bootStrapJob,tinePool(bootStrapJob)))
        } else {
          println(s"Holder $holderIndex Lost Connection with Tine Provider")
          tinePool -= bootStrapJob
          bootStrapJob = -1
          bootStrapLock = false
        }
      } else {
        println(s"Holder $holderIndex Bootstrap Job not in Tinepool")
        bootStrapJob = -1
        bootStrapLock = false
      }


    /**issue a transaction generated by the coordinator and send it to the list of gossipers*/
    case value:IssueTx =>
      if (!actorStalled) Try{
        inbox.toSeq.find(_._2._1 == value.ref) match {
          case Some(data:(Sid,(ActorRefWrapper,PublicKeys))) =>
            val pks = data._2._2
              val pkw = ByteArrayWrapper(pks._1 ++ pks._2 ++ pks._3)
              wallet.issueTx((pkw,value.delta),keys.sk_sig,sig,rng,serializer) match {
                case Some(trans:Transaction) =>
                  walletStorage.store(wallet,serializer)
                  txCounter += 1
                  memPool += (trans.sid->(trans,0))
                  send(selfWrapper,gossipSet(selfWrapper,holders), SendTx(trans,selfWrapper))
                case _ =>
            }
          case None =>
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }

    case value:IssueTxToAddress =>
      if (!actorStalled) {
        wallet.issueTx((value.recip,value.delta),keys.sk_sig,sig,rng,serializer) match {
          case Some(trans:Transaction) =>
            walletStorage.store(wallet,serializer)
            txCounter += 1
            memPool += (trans.sid->(trans,0))
            send(selfWrapper,gossipSet(selfWrapper,holders), SendTx(trans,selfWrapper))
          case _ =>
        }
      }
      if (useFencing) {
        routerRef ! Flag(selfWrapper,"passData")
      }

    /**sends holder information for populating inbox*/
    case Diffuse =>
      send(selfWrapper,gossipSet(selfWrapper,holders),DiffuseData(sessionId,selfWrapper,keys.publicKeys,selfWrapper))

    /**allocation and vars of simulation*/
    case Initialize(gs,inputPassword) =>
      globalSlot = gs
      println("Holder "+holderIndex.toString+s" starting on global slot $globalSlot")
      inputPassword match {
        case Some(pw) => password = pw
        case None if password == "" => password = s"password_holder_$holderIndex"
        case _ =>
      }
      salt = fch.hash(uuid)
      derivedKey = KeyFile.getDerivedKey(password,salt)

      def generateNewKeys():Unit = {
        println("Generating new keyfile...")
        val rngSeed:Random = new Random
        rngSeed.setSeed(BigInt(seed).toLong)
        val seed1 = fch.hash(rngSeed.nextString(32))
        val seed2 = fch.hash(rngSeed.nextString(32))
        val seed3 = fch.hash(rngSeed.nextString(32))
        keyFile = Some(KeyFile.fromSeed(
          password,
          s"$storageDir/keys/",
          serializer,
          sig:Sig,
          vrf:Vrf,
          kes:Kes,
          0,
          seed1,
          seed2,
          seed3
        ))
      }

      keyFile match {
        case None => Try{KeyFile.restore(s"$storageDir/keys/")} match {
          case Success(Some(restoredFile:KeyFile)) =>
            println("Reading Keyfile")
            keyFile = Some(restoredFile)
          case Success(None) => generateNewKeys()
          case Failure(exception) =>
            exception.printStackTrace()
            generateNewKeys()
        }
        case _ =>
      }
      keys = keyFile.get.getKeys(password,serializer,sig,vrf,kes)
      wallet = walletStorage.restore(serializer,keys.pkw)
      chainStorage.restore(localChainId,serializer) match {
        case newChain:Tine if newChain.isEmpty =>
          localChain.update((0,genBlockHash),genesisBlock.get.blockHeader.get._5)
          updateLocalState(localState, (0,genBlockHash)) match {
            case Some(value:State) => localState = value
            case _ =>
              SharedData.throwError(holderIndex)
              println("error: invalid genesis block")
          }
          eta = eta_from_genesis(localChain, 0)
          println("Adding genesis state to history")
          history.add((0,genBlockHash),localState,eta)
          stakingState = getStakingState(currentEpoch,localChain)
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
          updateWallet()
        case newChain:Tine if !newChain.isEmpty =>
          localChain = newChain
          val lastId = localChain.last
          localSlot = localChain.last._1
          currentEpoch = localSlot/epochLength
          val loadState = history.get(lastId).get
          localState = loadState._1
          eta = loadState._2
          stakingState = getStakingState(currentEpoch,localChain)
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
      }
      keys.alpha = alphaCache.get.get(keys.pkw)
      assert(genBlockHash == hash(genesisBlock.get.blockHeader.get,serializer))
      println("Valid Genesis Block")
      sender() ! "done"

    /**starts the timer that repeats the update command*/
    case Run =>
      if (holderIndex == SharedData.printingHolder && useGui) Try{
        val win = SharedData.prosomoWindow.get
        win.pendingTxField.get.enabled = true
        win.issueTxButton.get.enabled = true
      }
      if (!useFencing) {
        context.system.scheduler.scheduleOnce(updateTime,self,Update)(context.system.dispatcher,self)
        timers.startPeriodicTimer(TimerKey, GetTime, updateTime)
        context.system.scheduler.scheduleOnce(slotT* (refreshInterval * rng.nextDouble).toInt.millis,self,Refresh)(context.system.dispatcher,self)
        context.system.scheduler.scheduleOnce(5*slotT.millis,self,Diffuse)(context.system.dispatcher,self)
      }
      self ! BootstrapJob


    case Refresh =>
      blocks.refresh
      chainStorage.refresh
      history.refresh
      walletStorage.refresh
      context.system.scheduler.scheduleOnce(slotT*refreshInterval.millis,self,Refresh)(context.system.dispatcher,self)

    case GetTime =>
      coordinatorRef ! GetTime

    /**sets the initial time*/
    case value:SetClock =>
      t0 = value.t0
      sender() ! "done"

    /**sets the slot from coordinator time*/
    case value:GetTime => if (!actorStalled) {
      t1 = value.t1
      globalSlot = ((value.t1 - t0) / slotT).toInt
    }

    /**accepts list of other holders from coordinator */
    case HoldersFromLocal(list:List[ActorRefWrapper]) =>
      holders = list
      sender() ! "done"

    /**accepts genesis block from coordinator */
    case gb:GenBlock =>
      genBlockHash = hash(gb.b.prosomoHeader,serializer)
      println("Holder "+holderIndex.toString+" got genesis block "+Base58.encode(genBlockHash.data))
      assert(genBlockHash == gb.b.id)
      assert(verifyBlock(gb.b))
      genesisBlock = Some(gb.b)
      if (!blocks.knownIfPresent((0,gb.b.id))){
        blocks.add(gb.b)
      }
      sender() ! "done"

    /**accepts coordinator ref*/
    case CoordRef(ref) =>
      coordinatorRef = ref
      sender() ! "done"

    /**sets new list of holders resets gossipers*/
    case Party(list,clear) =>
      holders = list
      if (clear) inbox = Map()
      sender() ! "done"

    /************************************* Research and Testing ******************************************/

    case RequestState =>
      sender() ! GetState(stakingState)

    case RequestBlockTree =>
      sender() ! GetBlockTree(blocks,0)

    /**when stalled actor will do nothing when messages are received*/
    case StallActor =>
      if (!actorStalled) {actorStalled = true}
      else {actorStalled = false}
      sender() ! "done"

    /**prints inbox */
    case Inbox =>
      var i = 0
      println("Holder "+holderIndex.toString+" sid:"+Base58.encode(sessionId.data)+", Inbox:")
      for (entry <- inbox) {
        println(i.toString+" "+Base58.encode(entry._1.data))
        i+=1
      }
      println("Known holders:")
      holders.foreach(r=>println(r.actorPath.toString))
      sender() ! "done"

    case GetBalance =>
      val netAvailable = wallet.getConfirmedBalance
      val netTotal = wallet.getPendingBalance
      println(s"Holder $holderIndex available balance: $netAvailable , total balance: $netTotal")

    /**prints stats */
    case Verify =>
      val trueChain = verifyChain(localChain, genBlockHash)
      println("Holder "+holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toDouble + ", blocks forged = "
        + blocksForged.toString + "\nChain length = " + getActiveSlots(localChain).toString + ", Valid chain = "
        + trueChain.toString)
      var chainBytes:Array[Byte] = Array()
      for (id <- subChain(localChain,0,localSlot-confirmationDepth).ordered) {
        getBlockHeader(id) match {
          case Some(b:BlockHeader) => chainBytes ++= fch.hash(serializer.getBytes(b))
          case _ =>
        }
      }
      println("Public Key: "+Base58.encode(keys.pk_sig++keys.pk_vrf++keys.pk_kes))
      println("Path: "+self.path)
      println("Chain hash: " + Base58.encode(fch.hash(chainBytes))+"\n")
      if (SharedData.error){
        for (id <- localChain.ordered) {
          if (id._1 > -1) println("H:" + holderIndex.toString + "S:" + id._1.toString + "ID:" + Base58.encode(id._2.data))
        }
        println("e:" + Base58.encode(eta_from_genesis(localChain, currentEpoch)) + "\n")
      }
      sender() ! "done"

    /**prints stats */
    case Status =>
      println("Holder "+holderIndex.toString + ": t = " + localSlot.toString + ", alpha = " + keys.alpha.toDouble + ", blocks forged = "
        + blocksForged.toString + "\nChain length = " + getActiveSlots(localChain).toString+", MemPool Size = "+memPool.size)
      var chainBytes:Array[Byte] = Array()
      for (id <- subChain(localChain,0,localSlot-confirmationDepth).ordered) {
        getBlockHeader(id) match {
          case Some(b:BlockHeader) =>
            chainBytes ++= fch.hash(serializer.getBytes(b))
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
          }
        }
      }
      val holderTxCount = holderTxOnChain.length
      val txCountChain = if (holderTxOnChain.isEmpty) {0} else {holderTxOnChain.head._2.nonce}
      val txCountState = math.max(localState(keys.pkw)._3-1,0)
      println(s"Tx Counts in state and chain: $txCountState, $txCountChain")
      println(s"Transactions on chain: $holderTxCount/$txCount Duplicates: $duplicatesFound")
      println("Chain hash: " + Base58.encode(fch.hash(chainBytes))+"\n")
      sender() ! "done"

    /**writes data point to file*/
    case value:WriteFile => if (!actorStalled) {
      value.fw match {
        case fileWriter: BufferedWriter =>
          val fileString = (
            holderIndex.toString + " "
              + globalSlot.toString + " "
              + keys.alpha.toDouble + " "
              + blocksForged.toString + " "
              + getActiveSlots(localChain).toString + " "
              + "\n"
            )
          fileWriter.write(fileString)
        case _ => println("error: data file writer not initialized")
      }
    }


    case value:GetSlot =>
      if (!actorStalled) {
        if (roundBlock == 0) globalSlot += 1
        assert(globalSlot == value.s)
        while (roundBlock == 0) {
          roundBlock = roundBlock
          update()
        }
      } else {
        if (useFencing) {routerRef ! Flag(selfWrapper,"updateSlot")}
      }
      sender() ! "done"

    case "endStep" => if (useFencing) {
      roundBlock = 0
      routerRef ! Flag(selfWrapper,"endStep")
    }

    case "passData" => if (useFencing) {
      routerRef ! Flag(selfWrapper,"passData")
    }

    case value:Adversary =>
      value.s match {
        case "" =>
          if (adversary) {
            adversary=false
          } else {
            adversary=true
          }
        case "covert" =>
          if (covert) {
            adversary=false
            covert=false
          } else {
            adversary=true
            covert=true
          }
        case "nas" =>
          if (forgeAll) {
            adversary = false
            forgeAll = false
          } else {
            adversary = true
            forgeAll = true
          }
        case _ => println("error: Adversary command unknown")
      }
      sender() ! "done"


    case unknown:Any => if (!actorStalled) {
      print("Error: received unknown message ")
      println(unknown.getClass.toString)
    }
  }
}
