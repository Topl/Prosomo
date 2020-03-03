package prosomo.stakeholder

import prosomo.cases.{GetTime, Hello, WriteFile}
import prosomo.components.Chain
import prosomo.primitives.{KeyFile, Parameters, SharedData}
import scorex.crypto.encode.Base58
import akka.actor.Actor
import akka.actor.Props
import scala.concurrent.duration._

trait Update extends Members {
  import Parameters.{printFlag,epochLength,useGossipProtocol,numGossipers,dataOutInterval,dataOutFlag,useFencing}

  /**epoch routine, called every time currentEpoch increments*/
  def updateEpoch(slot:Slot,epochIn:Int):Int = {
    var ep = epochIn
    if (slot / epochLength > ep) {
      ep = slot / epochLength
      updateStakingState(ep)
      if (ep > 0) {
        eta = eta(localChain, ep, eta)
        //println(s"Holder $holderIndex set eta to "+Base58.encode(eta))
      }
    }
    ep
  }

  def updateStakingState(ep:Int):Unit = {
    stakingState = {
      if (ep > 1) {
        val eps:Slot = (ep-1)*epochLength
        history.get(localChain.getLastActiveSlot(eps)) match {
          case value:(State,Eta) =>
            value._1
          case _ =>
            val thisSlot = lastActiveSlot(localChain,eps)
            println(s"Could not recover staking state ep $ep slot $thisSlot id:"+Base58.encode(localChain.getLastActiveSlot(eps)._2.data))
            localChain.print
            SharedData.throwError(holderIndex)
            Map()
        }
      } else {
        history.get(localChain.get(0)) match {
          case value:(State,Eta) =>
            value._1
          case _ =>
            println("Could not recover staking state ep 0")
            SharedData.throwError(holderIndex)
            Map()
        }
      }
    }
    keys.alpha = relativeStake(keys.pkw, stakingState)
    keys.threshold = phi(keys.alpha)
  }

  def update:Unit = timeFlag{

    if (SharedData.error) actorStalled = true

    if (!actorStalled && !updating && diffuseSent) {

      updating = true
      
      if (globalSlot > tMax || SharedData.killFlag) {timers.cancelAll;updating = false}

      while (globalSlot > localSlot) {
        localSlot += 1
        if (dataOutFlag && localSlot % dataOutInterval == 0) {
          coordinatorRef ! WriteFile
        }
        if (holderIndex == SharedData.printingHolder) println(Console.CYAN + "Slot = " + localSlot.toString + " on block "
          + Base58.encode(localChain.getLastActiveSlot(localSlot)._2.data) + Console.WHITE)
        updateEpoch(localSlot,currentEpoch) match {
          case ep:Int if ep > currentEpoch => {
            currentEpoch = ep
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Current Epoch = " + currentEpoch.toString)
              println("Holder " + holderIndex.toString + " alpha = " + keys.alpha.toDoubleString+"\nEta:"+Base58.encode(eta))
            }
          }
          case _ =>
        }

        if (keys.sk_kes.time(kes) < globalSlot && globalSlot == localSlot && updating) {
          keys.sk_kes.update(kes, globalSlot)
          if (!useFencing) {
            forgeBlock(keys)
          }
          keyFile = KeyFile.update(keyFile,keys.sk_kes,password,storageDir,serializer,salt,derivedKey)
          if (useGossipProtocol) {
            val newOff = (numGossipers*math.sin(2.0*math.Pi*(globalSlot.toDouble/100.0+phase))/2.0).toInt
            if (newOff != gOff) {
              if (gOff < newOff) numHello = 0
              gOff = newOff
            }
            if (gossipers.length < numGossipers + gOff && numHello < 1) {
              send(self,rng.shuffle(holders.filter(_!=self)),Hello(self,signMac(hash(self,serializer), sessionId, keys.sk_sig, keys.pk_sig)))
              numHello += 1
            } else if (gossipers.length > numGossipers + gOff) {
              gossipers = rng.shuffle(gossipers).take(numGossipers + gOff)
            }
          }
        }
      }

      if (!useFencing) while (candidateTines.nonEmpty) {
        maxValidBG
      }

      if (useFencing) roundBlock match {
        case 0 => {
          forgeBlock(keys)
          roundBlock = 1
          routerRef ! (self,"updateSlot")
        }
        case _ if chainUpdateLock => {
          if (candidateTines.isEmpty) {
            chainUpdateLock = false
          } else {
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Checking Tine")
            }
            maxValidBG
          }
        }
        case _ =>
      }

      updating = false
    }
  }

}
