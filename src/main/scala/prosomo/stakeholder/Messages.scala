package prosomo.stakeholder

import akka.actor.{ActorPath, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import bifrost.crypto.hash.FastCryptographicHash
import prosomo.cases._
import prosomo.components.{BlockStorage, SlotReorgHistory}
import prosomo.primitives.{Box, Parameters}

import scala.concurrent.Await
import scala.math.BigInt

trait Messages extends Members {
  import Parameters._
  /**
    * Verifiable string for communicating between stakeholders
    * @param str data to be diffused
    * @param id holder identification information
    * @param sk_sig holder signature secret key
    * @return string to be diffused
    */
  def diffuse(str: String,id: String,sk_sig: PrivateKey): String = {
    str+";"+id+";"+bytes2hex(sig.sign(sk_sig,serializer.getBytes(str+";"+id)))
  }

  /**
    * Signed data box for verification between holders
    * @param data any data
    * @param id session id
    * @param sk_sig sig private key
    * @param pk_sig sig public key
    * @return signed box
    */
  def signBox(data: Hash, id:Sid, sk_sig: PrivateKey, pk_sig: PublicKey): Box = {
    new Box(data,id,sig.sign(sk_sig,data.data++id.data),pk_sig)
  }

  /**
    * verify a
    * @param box
    * @return
    */
  def verifyBox(input:Hash,box:Box): Boolean = {
    box.verify(input,sig,serializer)
  }


  /**
    * picks set of gossipers randomly
    * @param id self ref not to include
    * @param h list of holders
    * @return list of gossipers
    */
  def gossipSet(id:ActorPath,h:List[ActorRef]):List[ActorRef] = {
    var out:List[ActorRef] = List()
    for (holder <- rng.shuffle(h)) {
      if (holder.path != id && out.length < numGossipers) {
        out = holder::out
      }
    }
    out
  }

  /**
    * Sends command to one of the stakeholders
    * @param holder actor list
    * @param command object to be sent
    */
  def send(sender:ActorRef,holder:ActorRef,command: Any) = {
    if (useRouting && !useFencing) {
      routerRef ! (sender,holder,command)
    } else if (useFencing) {
      routerRef ! (BigInt(FastCryptographicHash(rng.nextString(64))),sender,holder,command)
    } else {
      holder ! command
    }
  }

  /**
    * Sends commands one by one to list of stakeholders
    * @param holders actor list
    * @param command object to be sent
    */
  def send(sender:ActorRef,holders:List[ActorRef],command: Any) = {
    for (holder <- holders){
      if (useRouting && !useFencing) {
        routerRef ! (sender, holder, command)
      } else if (useFencing) {
        routerRef ! (BigInt(FastCryptographicHash(rng.nextString(64))),sender,holder,command)
      } else {
        holder ! command
      }
    }
  }

  /**
    * Sends commands one by one to list of stakeholders
    * @param holders actor list
    * @param command object to be sent
    */
  def sendAssertDone(holders:List[ActorRef], command: Any) = {
    for (holder <- holders){
      implicit val timeout:Timeout = Timeout(waitTime)
      val future = holder ? command
      val result = Await.result(future, timeout.duration)
      assert(result == "done")
    }
  }

  /**
    * Sends command to stakeholder and waits for response
    * @param holder
    * @param command
    */
  def sendAssertDone(holder:ActorRef, command: Any) = {
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = holder ? command
    val result = Await.result(future, timeout.duration)
    assert(result == "done")
  }

  /**
    * returns map of gossipers to coordinator
    * @param holders
    * @return map of actor ref to its list of gossipers
    */
  def getGossipers(holders:List[ActorRef]):Map[ActorRef,List[ActorRef]] = {
    var gossipersMap:Map[ActorRef,List[ActorRef]] = Map()
    for (holder <- holders){
      implicit val timeout:Timeout = Timeout(waitTime)
      val future = holder ? RequestGossipers
      val result = Await.result(future, timeout.duration)
      result match {
        case value:GetGossipers => {
          value.list match {
            case l:List[ActorRef] => gossipersMap += (holder->l)
            case _ => println("error")
          }
        }
        case _ => println("error")
      }
    }
    gossipersMap
  }

  /**
    * returns the staking state to the coordinator
    * @param holder
    * @return
    */
  def getStakingState(holder:ActorRef):State = {
    var state:State = Map()
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = holder ? RequestState
    val result = Await.result(future, timeout.duration)
    result match {
      case value:GetState => {
        value.s match {
          case s:State => state = s
          case _ => println("error")
        }
      }
      case _ => println("error")
    }
    state
  }

  /**
    * sets the local chain history and block data to the holders
    * @param holder actor to get data from
    */
  def getBlockTree(holder:ActorRef) = {
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = holder ? RequestBlockTree
    val result = Await.result(future, timeout.duration)
    result match {
      case value:GetBlockTree => {
        value.t match {
          case t:BlockStorage => blocks.copy(t)
          case _ => println("error")
        }
        value.h match {
          case h:SlotReorgHistory => chainHistory.copy(h)
          case _ => println("error")
        }
      }
      case _ => println("error")
    }
  }

  def getPositionData(router:ActorRef):(Map[ActorRef,(Double,Double)],Map[(ActorRef,ActorRef),Long]) = {
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = router ? RequestPositionData
    val result = Await.result(future, timeout.duration)
    result match {
      case value:GetPositionData => {
        value.s match {
          case data:(Map[ActorRef,(Double,Double)],Map[(ActorRef,ActorRef),Long]) => {
            data
          }
        }
      }
    }
  }

  /**
    * Sends commands one by one to list of stakeholders
    * @param holders actor list
    * @param command object to be sent
    * @param input map of holder data
    * @return map of holder data
    */
  def collectKeys(holders:List[ActorRef], command: Any, input: Map[String,String]): Map[String,String] = {
    var list:Map[String,String] = input
    for (holder <- holders){
      implicit val timeout:Timeout = Timeout(waitTime)
      val future = holder ? command
      Await.result(future, timeout.duration) match {
        case str:String => {
          if (verifyStamp(str)) list = list++Map(s"${holder.path}" -> str)
        }
        case _ => println("error")
      }
    }
    list
  }

  /**
    * send diffuse message between holders, used for populating inbox
    * @param holderId
    * @param holders
    * @param command
    */
  def sendDiffuse(holderId:ActorPath, holders:List[ActorRef], command: Any) = {
    for (holder <- holders){
      implicit val timeout:Timeout = Timeout(waitTime)
      if (holder.path != holderId) {
        val future = holder ? command
        val result = Await.result(future, timeout.duration)
        assert(result == "done")
      }
    }
    diffuseSent = true
  }


  /**
    * Verify diffused strings with public key included in the string
    * @param value string to be checked
    * @return true if signature is valid, false otherwise
    */
  def verifyStamp(value: String): Boolean = {
    val values: Array[String] = value.split(";")
    val m = values(0) + ";" + values(1) + ";" + values(2) + ";" + values(3)
    sig.verify(hex2bytes(values(4)), serializer.getBytes(m), hex2bytes(values(0)))
  }


}
