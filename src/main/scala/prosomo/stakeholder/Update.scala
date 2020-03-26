package prosomo.stakeholder

import prosomo.cases.{Hello, WriteFile}
import prosomo.components.Tine
import prosomo.primitives.{KeyFile, Parameters, SharedData}
import scorex.crypto.encode.Base58
import akka.actor.Actor
import akka.actor.Props
import scala.concurrent.duration._

trait Update extends Members {
  import Parameters.{printFlag,epochLength,useGossipProtocol,numGossipers,dataOutInterval,dataOutFlag,useFencing}

  /**epoch routine, called every time currentEpoch increments*/
  def updateEpoch(slot:Slot,epochIn:Int,lastEta:Eta,chain:Tine):(Int,Eta) = {
    val ep = slot / epochLength
    if (ep > epochIn) {
      //println(s"Holder $holderIndex old eta "+Base58.encode(eta))
      val newEta = eta_from_tine(chain, ep, lastEta)
      //println(s"Holder $holderIndex eta now "+Base58.encode(newEta))
      (epochIn+1,newEta)
    } else {
      (epochIn,lastEta)
    }
  }

  def getStakingState(ep:Int, chain:Tine):State = if (ep > 1) {
    val eps:Slot = (ep-1)*epochLength
    history.get(chain.getLastActiveSlot(eps)) match {
      case value:(State,Eta) =>
        value._1
      case _ =>
        val thisSlot = lastActiveSlot(chain,eps)
        println(s"Could not recover staking state ep $ep slot $thisSlot id:"+Base58.encode(localChain.getLastActiveSlot(eps)._2.data))
        chain.print
        SharedData.throwError(holderIndex)
        Map()
    }
  } else {
    history.get(chain.get(0)) match {
      case value:(State,Eta) =>
        value._1
      case _ =>
        println("Could not recover staking state ep 0")
        SharedData.throwError(holderIndex)
        Map()
    }
  }

  def update:Unit = timeFlag{

    if (SharedData.error) actorStalled = true

    if (!actorStalled && !updating) {

      updating = true
      
      if (globalSlot > tMax || SharedData.killFlag) {
        timers.cancelAll
        updating = false
      }

      while (globalSlot > localSlot) {
        localSlot += 1
        if (dataOutFlag && localSlot % dataOutInterval == 0) {
          coordinatorRef ! WriteFile
        }
        if (holderIndex == SharedData.printingHolder) println(Console.CYAN + "Slot = " + localSlot.toString + " on block "
          + Base58.encode(localChain.getLastActiveSlot(localSlot)._2.data) + Console.WHITE)
        updateEpoch(localSlot,currentEpoch,eta,localChain) match {
          case result:(Int,Eta) if result._1 > currentEpoch => {
            currentEpoch = result._1
            eta = result._2
            stakingState = getStakingState(currentEpoch,localChain)
            keys.alpha = relativeStake(keys.pkw,stakingState)
            keys.threshold = phi(keys.alpha)
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
              send(ActorRefWrapper(self),rng.shuffle(holders.filter(_!=ActorRefWrapper(self))),Hello(ActorRefWrapper(self),signMac(hash(ActorRefWrapper(self),serializer), sessionId, keys.sk_sig, keys.pk_sig)))
              numHello += 1
            } else if (gossipers.length > numGossipers + gOff) {
              gossipers = rng.shuffle(gossipers).take(numGossipers + gOff)
            }
          }
        }
      }

      if (!useFencing) while (candidateTines.nonEmpty && updating) {
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
