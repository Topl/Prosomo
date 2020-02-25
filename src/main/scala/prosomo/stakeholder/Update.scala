package prosomo.stakeholder

import prosomo.cases.{GetTime, Hello, WriteFile}
import prosomo.components.Chain
import prosomo.primitives.{KeyFile, Parameters, SharedData}
import scorex.crypto.encode.Base58

trait Update extends Members {
  import Parameters.{printFlag,epochLength,useGossipProtocol,numGossipers,dataOutInterval,dataOutFlag,useFencing}
  /**slot routine, called every time currentSlot increments*/
  def updateSlot = {
    updateEpoch(localSlot,currentEpoch) match {
      case ep:Int if ep > currentEpoch => currentEpoch = ep
      case _ =>
    }
    if (localSlot == globalSlot) {
      timeFlag(
        if (keys.sk_kes.time(kes) < localSlot) {
          if (holderIndex == SharedData.printingHolder && printFlag && localSlot%epochLength == 0) {
            println("Current Epoch = " + currentEpoch.toString)
            println("Holder " + holderIndex.toString + " alpha = " + keys.alpha.toDoubleString+"\nEta:"+Base58.encode(eta))
          }
          roundBlock = 0
          if (holderIndex == SharedData.printingHolder) println(Console.CYAN + "Slot = " + localSlot.toString + " on block "
            + Base58.encode(localChain.getLastActiveSlot(globalSlot)._2.data) + Console.WHITE)
          keys.sk_kes.update(kes, localSlot)
          keyFile = KeyFile.update(keyFile,keys.sk_kes,password,storageDir,serializer)
          if (useGossipProtocol) {
            val newOff = (numGossipers*math.sin(2.0*math.Pi*(globalSlot.toDouble/100.0+phase))/2.0).toInt
            if (newOff != gOff) {
              if (gOff < newOff) numHello = 0
              gOff = newOff
            }
            if (gossipers.length < numGossipers + gOff && numHello < 1) {
              send(self,rng.shuffle(holders.filter(_!=self)),Hello(self,signBox(hash(self,serializer), sessionId, keys.sk_sig, keys.pk_sig)))
              numHello += 1
            } else if (gossipers.length > numGossipers + gOff) {
              gossipers = rng.shuffle(gossipers).take(numGossipers + gOff)
            }
          }
        }
      )
      updateLocalState(localState, Chain(localChain.get(localSlot))) match {
        case value:State => localState = value
        case _ => {
          SharedData.throwError(holderIndex)
          println("error: invalid block ledger on chain")
        }
      }
      if (dataOutFlag && globalSlot % dataOutInterval == 0) {
        coordinatorRef ! WriteFile
      }
    }
  }

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
        history.get(localChain.getLastActiveSlot(eps)._2,serializer) match {
          case value:(State,Eta) => {
            value._1
          }
          case _ => {
            val thisSlot = lastActiveSlot(localChain,eps)
            println(s"Could not recover staking state ep $ep slot $thisSlot id:"+Base58.encode(localChain.getLastActiveSlot(eps)._2.data))
            localChain.print
            SharedData.throwError(holderIndex)
            Map()
          }
        }
      } else {
        history.get(localChain.get(0)._2,serializer) match {
          case value:(State,Eta) => {
            value._1
          }
          case _ => {
            println("Could not recover staking state ep 0")
            SharedData.throwError(holderIndex)
            Map()
          }
        }
      }
    }
    keys.alpha = relativeStake(keys.pkw, stakingState)
    keys.threshold = phi(keys.alpha)
  }

  def update = { if (SharedData.error) {actorStalled = true}
    if (!actorStalled) {
      if (!updating) {
        updating = true
        if (globalSlot > tMax || SharedData.killFlag) {
          timers.cancelAll
        } else if (diffuseSent) {
          if (!useFencing) coordinatorRef ! GetTime
          if (globalSlot > localSlot) {
            while (globalSlot > localSlot) {
              localSlot += 1
              updateSlot
            }
          } else if (roundBlock == 0 && candidateTines.isEmpty) {
            forgeBlock(keys)
            if (useFencing) {routerRef ! (self,"updateSlot")}
          } else if (!useFencing && candidateTines.nonEmpty) {
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Checking Tine")
            }
            timeFlag(maxValidBG)
          } else if (useFencing && chainUpdateLock) {
            if (candidateTines.isEmpty) {
              chainUpdateLock = false
            } else {
              if (holderIndex == SharedData.printingHolder && printFlag) {
                println("Holder " + holderIndex.toString + " Checking Tine")
              }
              timeFlag(maxValidBG)
            }
          }
        }
        updating = false
      }
    }
  }
}
