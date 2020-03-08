package prosomo.stakeholder

import java.io.BufferedWriter

import akka.actor.ActorRef
import bifrost.crypto.hash.FastCryptographicHash
import prosomo.cases._
import prosomo.components.{Block, Tine, Serializer, Transaction}
import prosomo.primitives.{Kes, KeyFile, Parameters, SharedData, Sig, Vrf}
import prosomo.wallet.Wallet
import scorex.crypto.encode.Base58

import scala.math.BigInt

trait Receive extends Members {
  import Parameters._
  def receive: Receive = {

    /**************************************************** Holders *********************************************************/

    /**updates time, the kes key, and resets variables */
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
              send(self,gossipers, SendTx(value.transaction))
            }
          }
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

    /**block passing, new blocks delivered are added to list of tines and then sent to gossipers*/
    case value:SendBlock => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.mac.sid)) {
          val foundBlock = blocks.known((value.block.slot,value.block.id))
          if (!foundBlock) {
            val b:BlockHeader = value.block.prosomoHeader
            val bHash = hash(b,serializer)
            val bSlot = b._3
            if (verifyMac(value.block.id,value.mac)) {
              if (verifyBlock(value.block)) {
                blocks.add(value.block)
                if (bSlot <= globalSlot) {
                  //if (holderIndex == SharedData.printingHolder && printFlag) {println("Holder " + holderIndex.toString + " Got New Tine")}
                  val newId = (bSlot, bHash)
                  send(self,gossipers, SendBlock(value.block,signMac(value.block.id, sessionId, keys.sk_sig, keys.pk_sig)))
                  val jobNumber = tineCounter
                  tines += (jobNumber -> (Tine(newId),0,0,0,inbox(value.mac.sid)._1))
                  buildTine((jobNumber,tines(jobNumber)))
                  tineCounter += 1
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
        routerRef ! (self,"passData")
      }
    }

    /**block passing, returned blocks are added to block database*/
    case value:ReturnBlock => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.mac.sid)) {
          def blockToId(b:Block):SlotId = (b.slot,b.id)
          if (verifyMac(hash((value.blocks.map(blockToId(_)),0,value.job),serializer),value.mac)) {
            for (block <- value.blocks) {
              if (!blocks.known(blockToId(block))) {
                if (verifyBlock(block)) {
                  if (holderIndex == SharedData.printingHolder && printFlag) {
                    println("Holder " + holderIndex.toString + " Got Block "+Base58.encode(block.id.data))
                  }
                  blocks.add(block)
                } else {println("error: invalid returned block")}
                if (tines.keySet.contains(value.job)) buildTine((value.job,tines(value.job)))
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
        routerRef ! (self,"passData")
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
            if (blocks.known_then_load(value.id)) {
              val returnedBlock:List[Block] = List(blocks.get(value.id))
              send(self,ref,ReturnBlock(returnedBlock,signMac(hash((List(value.id),0,value.job),serializer),sessionId,keys.sk_sig,keys.pk_sig),value.job))
              if (holderIndex == SharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Returned Block")
              }
            } else {
              send(self,ref,ReturnBlock(List(),signMac(hash((List(),0,value.job),serializer),sessionId,keys.sk_sig,keys.pk_sig),value.job))
            }
          } else {println("error: request block invalid mac")}
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

    /**block passing, parent ids are requested with increasing depth of chain upto a finite number of attempts*/
    case value:RequestChain => {
      if (!actorStalled) {
        if (inbox.keySet.contains(value.mac.sid)) {
          val request:Request = (List(value.id),value.depth,value.job)
          if (verifyMac(hash(request,serializer),value.mac) && value.depth <= tineMaxDepth) {
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Was Requested Blocks")
            }
            val ref = inbox(value.mac.sid)._1
            val startId:SlotId = value.id
            val depth:Int = value.depth
            val job:Int = value.job
            var returnedBlockList:List[Block] = List()
            var returnedIdList:List[SlotId] = List()
            var id:SlotId = startId
            while (returnedBlockList.length < k_s*depth && blocks.known_then_load(id)) {
              returnedBlockList ::= blocks.get(id)
              returnedIdList ::= id
              id = (returnedBlockList.head.parentSlot,returnedBlockList.head.pid)
            }
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Returned Blocks")
            }
            send(self,ref,ReturnBlock(returnedBlockList,signMac(hash((returnedIdList,0,value.job),serializer),sessionId,keys.sk_sig,keys.pk_sig),value.job))
          } else {println("error:chain request mac invalid")}
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

    /**issue a transaction generated by the coordinator and send it to the list of gossipers*/
    case value:IssueTx => {
      if (!actorStalled) {
        value.s match {
          case data:(PublicKeyW,BigInt) => {
            //if (printFlag) {println(s"Holder $holderIndex Issued Transaction:"+"local state balance:"+localState(keys.pkw)._1.toString)}
            wallet.issueTx(data,keys.sk_sig,sig,rng,serializer) match {
              case trans:Transaction => {
                walletStorage.store(wallet,serializer)
                txCounter += 1
                //setOfTxs += (trans.sid->trans.nonce)
                memPool += (trans.sid->(trans,0))
                send(self,gossipers, SendTx(trans))
              }
              case _ => //{println("Holder "+holderIndex.toString+" tx issue failed")}
            }
          }
          case _ => {println("invalid tx data");SharedData.throwError(holderIndex)}
        }
      } else {
        println("tx issued while stalled");SharedData.throwError(holderIndex)
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }

    /**gossip protocol greeting message for populating inbox*/
    case value:Hello => {
      if (!actorStalled) {
        if (gossipers.length < numGossipers + gOff) {
          if (verifyMac(hash(value.id,serializer),value.mac)) {
            if (!gossipers.contains(value.id) && inbox.keySet.contains(value.mac.sid)) {
              //if (holderIndex == SharedData.printingHolder && printFlag) {println("Holder " + holderIndex.toString + " Adding Gossiper")}
              if (inbox(value.mac.sid)._1 == value.id) gossipers = gossipers ++ List(value.id)
              send(self,value.id,Hello(self,signMac(hash(self,serializer), sessionId, keys.sk_sig, keys.pk_sig)))
            }
          }
        }
      }
      if (useFencing) {
        routerRef ! (self,"passData")
      }
    }



    /************************************************** From Coordinator *******************************************************/

    /**sends holder information for populating inbox*/
    case Diffuse => {
      sendDiffuse(holderId, holders, DiffuseData(self,keys.publicKeys,signMac(hash((self,keys.publicKeys),serializer), sessionId, keys.sk_sig, keys.pk_sig)))
      sender() ! "done"
    }

    /**validates diffused string from other holders and stores in inbox */
    case value:DiffuseData => {
      if (verifyMac(hash((value.ref,value.pks),serializer),value.mac) && !inbox.keySet.contains(value.mac.sid)) {
        inbox += (value.mac.sid->(value.ref,value.pks))
      }
      sender() ! "done"
    }

    /**allocation and vars of simulation*/
    case value:Initialize => {
      println("Holder "+holderIndex.toString+" starting...")
      tMax = value.tMax
      globalSlot = kes.getKeyTimeStep(keys.sk_kes)
      localSlot = globalSlot
      chainStorage.restore(localChainId,serializer) match {
        case newChain:Tine if newChain.isEmpty => {
          localChain.update((0,genBlockHash))
          chainHistory.update((0,genBlockHash),serializer)
          updateLocalState(localState, Tine(localChain.get(0))) match {
            case value:State => localState = {
              value
            }
            case _ => {
              SharedData.throwError(holderIndex)
              println("error: invalid genesis block")
            }
          }
          eta = eta(localChain, 0, Array())
          println("Adding genesis state to history")
          history.add((0,genBlockHash),localState,eta)
          updateWallet
          trimMemPool
        }
        case newChain:Tine if !newChain.isEmpty => {
          localChain.copy(newChain)
          val loadState = history.get(localChain.last) match {case data:(State,Eta) => data}
          localState = loadState._1
          eta = loadState._2
        }
      }
      val genesisBlock = blocks.get((0,genBlockHash))
      assert(genBlockHash == hash(genesisBlock.prosomoHeader,serializer))
      println("Valid Genesis Block")
      sender() ! "done"
    }

    /**starts the timer that repeats the update command*/
    case Run => {
      if (!useFencing) {
        context.system.scheduler.scheduleOnce(updateTime,self,Update)(context.system.dispatcher,self)
        timers.startPeriodicTimer(timerKey, GetTime, updateTime)
      }
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
    case list:List[ActorRef] => {
      holders = list
      if (useGossipProtocol) {
        gossipers = List()
      } else {
        gossipers = gossipSet(holderId,holders)
      }
      var i = 0
      for (holder <- holders) {
        if (self == holder) holderIndex = i
        i += 1
      }
      sender() ! "done"
    }

    /**accepts genesis block from coordinator */
    case gb:GenBlock => {
      genBlockHash = hash(gb.b.prosomoHeader,serializer)
      println("Holder "+holderIndex.toString+" got genesis block "+Base58.encode(genBlockHash.data))
      assert(genBlockHash == gb.b.id)
      assert(verifyBlock(gb.b))
      if (!blocks.known((0,gb.b.id))){
        blocks.add(gb.b)
      }
      sender() ! "done"
    }

    /**accepts coordinator ref*/
    case value:CoordRef => {
      coordinatorRef = value.ref
      sender() ! "done"
    }

    /**accepts router ref*/
    case value:RouterRef => {
      routerRef = value.ref
      sender() ! "done"
    }

    /**sets new list of holders resets gossipers*/
    case value:Party => {
      value.list match {
        case list: List[ActorRef] => {
          holders = list
          if (useGossipProtocol) {
            gossipers = List()
            numHello = 0
          } else {
            gossipers = gossipSet(holderId,holders)
          }
          if (value.clear) inbox = Map()
          diffuseSent = false
        }
        case _ =>
      }
      sender() ! "done"
    }

    case RequestKeys => {
      password = s"password_holder_$holderIndex"
      salt = FastCryptographicHash(uuid)
      derivedKey = KeyFile.getDerivedKey(password,salt)
      KeyFile.restore(storageDir) match {
        case Some(restoredFile:KeyFile) => {
          println("Reading keyfile ...")
          keyFile = restoredFile
        }
        case None => {
          println("Generating new keyfile...")
          val seed1 = FastCryptographicHash(rng.nextString(32))
          val seed2 = FastCryptographicHash(rng.nextString(32))
          val seed3 = FastCryptographicHash(rng.nextString(32))
          keyFile = KeyFile.fromSeed(
            password,
            storageDir,
            serializer: Serializer,
            sig:Sig,
            vrf:Vrf,
            kes:Kes,
            globalSlot:Slot,
            seed1,
            seed2,
            seed3
          )
        }
      }
      keys = keyFile.getKeys(password,serializer,sig,vrf,kes)
      wallet = walletStorage.restore(serializer,keys.pkw,fee_r)
      sender() ! diffuse(bytes2hex(keys.pk_sig)+";"+bytes2hex(keys.pk_vrf)+";"+bytes2hex(keys.pk_kes), s"{$holderId}", keys.sk_sig)
    }

    /************************************************** Tests ***********************************************************/

    case RequestGossipers => {
      sender() ! GetGossipers(gossipers)
    }

    case RequestState => {
      sender() ! GetState(stakingState)
    }

    case RequestBlockTree => {
      sender() ! GetBlockTree(blocks,chainHistory)
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
      println("Holder "+holderIndex.toString+":"+Base58.encode(sessionId.data))
      for (entry <- inbox) {
        println(i.toString+" "+Base58.encode(entry._1.data))
        i+=1
      }
      println("")
      sender() ! "done"
    }

    case GetBalance => {
      val netAvailable = wallet.getBalance
      val netTotal = wallet.getTotalBalance
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
          case b:BlockHeader => chainBytes ++= FastCryptographicHash(serializer.getBytes(b))
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
        println("e:" + Base58.encode(eta(localChain, currentEpoch)) + "\n")
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
          case b:BlockHeader => {
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
        for (trans<-blocks.getTxs(id)) {
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
        if (useFencing) {routerRef ! (self,"updateSlot")}
      }
      sender() ! "done"
    }

    case "endStep" => if (useFencing) {
      roundBlock = 0
      routerRef ! (self,"endStep")
    }

    case "passData" => if (useFencing) {
      routerRef ! (self,"passData")
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
