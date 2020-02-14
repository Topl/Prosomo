package prosomo.components

import akka.actor.{ActorPath, ActorRef}
import akka.util.Timeout
import akka.pattern.ask
import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.{GetBlockTree, GetGossipers, GetPositionData, GetState}
import prosomo.history.History
import prosomo.primitives.{Kes, Sig, Vrf, SharedData,Parameters,Ratio}
import scorex.crypto.encode.Base58
import prosomo.cases._

import scala.collection.immutable.ListMap
import scala.concurrent.Await
import scala.math.BigInt
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}

trait Methods extends Types with Parameters {

  val serializer:Serializer
  //vars for chain, blocks, state, history, and locks
  var localChain:Chain
  var blocks:BlockData
  var chainHistory:SlotReorgHistory
  var localState:State
  var eta:Eta
  var stakingState:State
  var memPool:MemPool
  var holderIndex:Int
  var diffuseSent:Boolean

  //verification and signing objects
  val vrf:Vrf
  val kes:Kes
  val sig:Sig

  val history:History
  //val mempool:Mempool
  var rng:Random
  var routerRef:ActorRef

  /**
    * retrieve parent block id from block
    * @param b
    * @return parent id
    */
  def getParentId(b:BlockHeader): SlotId = {
    (b._10,b._1)
  }

  /**
    * finds the last non-empty slot in a chain
    * @param c chain of block ids
    * @param s slot to start search
    * @return last active slot found on chain c starting at slot s
    */
  def lastActiveSlot(c:Chain, s:Slot): Slot = {
    var i:Slot = -1
    for (slot <- c.slots) {
      if (slot > i && slot <= s) i = slot
    }
    i
  }

  /**
    * returns the total number of active slots on a chain
    * @param c chain of block ids
    * @return total active slots
    */
  def getActiveSlots(c:Chain): Int = {
    c.slots.size
  }

  /**
    * returns a sub-chain containing all blocks in a given time interval
    * @param c input chain
    * @param t1 slot lower bound
    * @param t2 slot upper bound
    * @return all blocks in the interval t1 to t2, including blocks of t1 and t2
    */

  def subChain(c:Chain, t1:Int, t2:Int): Chain = {
    var t_lower:Int = 0
    var t_upper:Int = 0
    if (t1>0) t_lower = t1
    if (t2>0) t_upper = t2
    c.slice(t_lower,t_upper+1)
  }


  /**
    * Aggregate staking function used for calculating threshold per epoch
    * @param a relative stake
    * @return probability of being elected slot leader
    */
  def phi(a:Ratio): Ratio = {
    var out = Ratio(0)
    val base = maclaurin_coefficient * a
    for (n <- 1 to o_n) {
      out = out - ( base.pow(n) / factorial(n) )
    }
//    if (holderIndex == 0) {
      //val alpha = a.toBigDecimal.toDouble
//      val phiDouble = 1.0 - scala.math.pow(1.0 - f_s,alpha)
//      println(a.toString)
//      println(maclaurin_coefficient.toString)
//      println(s"alpha double:$alpha")
//      println(s"phi double:$phiDouble")
//      println(s"phi Ratio :${out.toBigDecimal.toDouble}")
//    }
    out
  }

  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n-1)
  }

  /**
    * Compares the vrf output to the threshold
    * @param y vrf output bytes
    * @param t threshold between 0.0 and 1.0
    * @return true if y mapped to double between 0.0 and 1.0 is less than threshold
    */
  def compare(y: Array[Byte],t: Ratio):Boolean = {
    var net:Ratio = Ratio(0)
    var i:Int = 0
    for (byte <- y){
      i += 1
      val n = BigInt(byte & 0xff)
      val d = BigInt(2).pow(8*i)
      net = net + new Ratio(n,d)
    }
//    if (holderIndex == 0) {
//      println(s"net:${net.toBigDecimal.toDouble}")
//      println(s"thr:${t.toBigDecimal.toDouble}")
//      println(net < t)
//    }
    net < t
  }

  /**
    * calculates alpha, the epoch relative stake, from the staking state
    * @param holderKey
    * @param ls
    * @return
    */
  def relativeStake(holderKey:PublicKeyW,ls:State): Ratio = {
    var netStake:BigInt = 0
    var holderStake:BigInt = 0
    for (member <- ls.keySet) {
      val (balance,activityIndex,txC) = ls(member)
      if (activityIndex) netStake += balance
    }
    if (ls.keySet.contains(holderKey)){
      val (balance,activityIndex,txC) = ls(holderKey)
      if (activityIndex) holderStake += balance
    }
    if (netStake > 0) {
      new Ratio(holderStake,netStake)
    } else {
      new Ratio(BigInt(0),BigInt(1))
    }
  }

  def uuid: String = java.util.UUID.randomUUID.toString

  def bytes2hex(b: Array[Byte]): String = {
    b.map("%02x" format _).mkString
  }

  def hex2bytes(hex: String): Array[Byte] = {
    if (hex.contains(" ")) {
      hex.split(" ").map(Integer.parseInt(_, 16).toByte)
    } else if (hex.contains("-")) {
      hex.split("-").map(Integer.parseInt(_, 16).toByte)
    } else {
      hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    }
  }

  def containsDuplicates(s:Map[String,String]):Boolean = {
    var s1:List[String] = List()
    var s2:List[String] = List()
    for (entry <- s) {
      s1 ++= List(entry._1)
      s2 ++= List(entry._2)
    }
    (s1.distinct.size != s1.size) && (s2.distinct.size != s2.size)
  }

  /**
    * retrieve a block header from database
    * @param bid
    * @return block if found, 0 otherwise
    */
  def getBlockHeader(bid:SlotId): Any = {
    if (bid._1 >= 0 && !bid._2.data.isEmpty) {
      if (blocks.known(bid)) {
        blocks.get(bid).header
      } else {
        0
      }
    } else {
      0
    }
  }

  /**
    * retrieve parent block
    * @param b
    * @return parent block if found, 0 otherwise
    */
  def getParentBlockHeader(b:BlockHeader): Any = {
    if (b._10 >= 0 && !b._1.data.isEmpty) {
      if (blocks.known(b._1)) {
        blocks.get(b._1).header
      } else {
        0
      }
    } else {
      0
    }
  }

  /**
    * retrieve parent block id
    * @param bid
    * @return parent id if found, 0 otherwise
    */
  def getParentId(bid:SlotId): Any = {
    getBlockHeader(bid) match {
      case b:BlockHeader => (b._10,b._1)
      case _ => 0
    }
  }

  /**
    * calculates epoch nonce recursively
    * @param c local chain to be verified
    * @param ep epoch derived from time step
    * @return hash nonce
    */
  def eta(c:Chain, ep:Int): Eta = {
    if(ep == 0) {
      getBlockHeader(c.get(0)) match {
        case b:BlockHeader => b._1.data
        case _ => Array()
      }
    } else {
      var v: Array[Byte] = Array()
      val epcv = subChain(c,ep*epochLength-epochLength,ep*epochLength-epochLength/3)
      val cnext = subChain(c,0,ep*epochLength-epochLength)
      for(id <- epcv.ordered) {
        getBlockHeader(id) match {
          case b:BlockHeader => v = v++b._5
          case _ =>
        }
      }
      FastCryptographicHash(eta(cnext,ep-1)++serializer.getBytes(ep)++v)
    }
  }

  /**
    * calculates epoch nonce from previous nonce
    * @param c local chain to be verified
    * @param ep epoch derived from time step
    * @param etaP previous eta
    * @return hash nonce
    */
  def eta(c:Chain, ep:Int, etaP:Eta): Eta = {
    //println(s"Holder $holderIndex:eta in:"+Base58.encode(etaP))
    if(ep == 0) {
      getBlockHeader(c.get(0)) match {
        case b:BlockHeader => b._1.data
        case _ => Array()
      }
    } else {
      var v: Array[Byte] = Array()
      val epcv = subChain(c,ep*epochLength-epochLength,ep*epochLength-epochLength/3)
      for(id <- epcv.ordered) {
        getBlockHeader(id) match {
          case b:BlockHeader => v = v++b._5
          case _ =>
        }
      }
      val eta_ep = FastCryptographicHash(etaP++serializer.getBytes(ep)++v)
      //println(s"Holder $holderIndex:eta out:"+Base58.encode(eta_ep))
      eta_ep
    }
  }

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
  def signBox(data: Any, id:Sid, sk_sig: PrivateKey, pk_sig: PublicKey): Box = {
    new Box(data,id,sig.sign(sk_sig,serializer.getAnyBytes(data)++id.data),pk_sig)
  }

  /**
    * verify a
    * @param box
    * @return
    */
  def verifyBox(box:Box): Boolean = {
    box.verify(sig,serializer)
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
          case t:BlockData => blocks = t
          case _ => println("error")
        }
        value.h match {
          case h:SlotReorgHistory => chainHistory = h
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
  def sendDiffuse(holderId:ActorPath, holders:List[ActorRef], command: Box) = {
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
    * Block verify using key evolving signature
    * @param b input block
    * @returnt true if signature is valid, false otherwise
    */
  def verifyBlock(b:BlockHeader): Boolean = {
    val (hash, ledger, slot, cert, rho, pi, sig, pk_kes, bn,ps) = b
    val kesVer = kes.verify(
      pk_kes,
      hash.data++serializer.getBytes(ledger)
        ++serializer.getBytes(slot)
        ++serializer.getBytes(cert)
        ++rho++pi++serializer.getBytes(bn)
        ++serializer.getBytes(ps),
      sig,
      slot
    )
    if (slot > 0) {
      kesVer && ledger.length <= txPerBlock + 1
    } else {
      kesVer
    }
  }

  /**
    * Verify chain using key evolving signature, VRF proofs, and hash id
    * @param c chain to be verified
    * @param gh genesis block hash
    * @return true if chain is valid, false otherwise
    */
  def verifyChain(c:Chain, gh:Hash): Boolean = {
    var bool = true
    var ep = -1
    var alpha_Ep:Ratio = new Ratio(BigInt(0),BigInt(1))
    var tr_Ep:Ratio = new Ratio(BigInt(0),BigInt(1))
    var eta_Ep: Eta = eta(c, 0)
    var stakingState: State = Map()
    var pid:SlotId = (0,gh)
    var i = 0

    getBlockHeader(c.get(0)) match {
      case b:BlockHeader => bool &&= hash(b,serializer) == gh
      case _ => bool &&= false
    }

    for (id <- c.ordered.tail) {
      getBlockHeader(id) match {
        case b:BlockHeader => {
          getParentBlockHeader(b) match {
            case pb:BlockHeader => {
              bool &&= getParentId(b) == pid
              if (getParentId(b) != pid) println("Holder "+holderIndex.toString+" pid mismatch")
              compareBlocks(pb,b)
              pid = id
            }
            case _ => bool &&= false
          }
        }
        case _ =>
      }
    }

    def compareBlocks(parent: BlockHeader, block: BlockHeader) = {
      val (h0, _, slot, cert, rho, pi, _, pk_kes, bn, ps) = block
      val (pk_vrf, y, pi_y, pk_sig, tr_c,_) = cert
      while(i<=slot) {
        if (i/epochLength > ep) {
          ep = i/epochLength
          eta_Ep = eta(c, ep, eta_Ep)
          updateLocalState(
            if(ep ==1) {Map()} else {stakingState}
            ,subChain(c,(i/epochLength)*epochLength-2*epochLength+1,(i/epochLength)*epochLength-epochLength)) match {
            case value:State =>  stakingState = value
            case _ => {
              println("Error: encountered invalid ledger in local chain")
              bool &&= false
            }
          }
        }
        i+=1
      }
      alpha_Ep = relativeStake(ByteArrayWrapper(pk_sig++pk_vrf++pk_kes), stakingState)
      tr_Ep = phi(alpha_Ep)
      bool &&= (
        hash(parent,serializer) == h0
          && verifyBlock(block)
          && parent._3 == ps
          && parent._9 + 1 == bn
          && vrf.vrfVerify(pk_vrf, eta_Ep ++ serializer.getBytes(slot) ++ serializer.getBytes("NONCE"), pi)
          && vrf.vrfProofToHash(pi).deep == rho.deep
          && vrf.vrfVerify(pk_vrf, eta_Ep ++ serializer.getBytes(slot) ++ serializer.getBytes("TEST"), pi_y)
          && vrf.vrfProofToHash(pi_y).deep == y.deep
          && tr_Ep == tr_c
          && compare(y, tr_Ep)
        )
      if (!bool) {
        print(slot)
        print(" ")
        println(Seq(
          hash(parent,serializer) == h0 //1
          , verifyBlock(block) //2
          , parent._3 == ps //3
          , parent._9 + 1 == bn //4
          , vrf.vrfVerify(pk_vrf, eta_Ep ++ serializer.getBytes(slot) ++ serializer.getBytes("NONCE"), pi) //5
          , vrf.vrfProofToHash(pi).deep == rho.deep //6
          , vrf.vrfVerify(pk_vrf, eta_Ep ++ serializer.getBytes(slot) ++ serializer.getBytes("TEST"), pi_y) //7
          , vrf.vrfProofToHash(pi_y).deep == y.deep //8
          , tr_Ep == tr_c //9
          , compare(y, tr_Ep) //10
        ))
      }
    }
    bool
  }

  /**
    * Verify chain using key evolving signature, VRF proofs, and hash rule
    * @param tine chain to be verified
    * @return true if chain is valid, false otherwise
    */
  def verifySubChain(tine:Chain, prefix:Slot): Boolean = {
    var isValid = true
    val ep0 = prefix/epochLength
    var eta_Ep:Eta = Array()
    var ls:State = Map()

    history.get(localChain.getLastActiveSlot(prefix)._2) match {
      case value:(State,Eta) => {
        ls = value._1
        eta_Ep = value._2
      }
      case _ => isValid &&= false
    }

    var stakingState: State = {
      if (ep0 > 1) {
        history.get(localChain.getLastActiveSlot((ep0-1)*epochLength)._2) match {
          case value:(State,Eta) => {
            value._1
          }
          case _ => {
            println("Error: staking state recovery failed")
            isValid &&= false
            Map()
          }
        }
      } else {
        history.get(localChain.get(0)._2) match {
          case value:(State,Eta) => {
            value._1
          }
          case _ => {
            println("Error: staking genesis state recovery failed")
            isValid &&= false
            Map()
          }
        }
      }
    }

    var ep = ep0
    var alpha_Ep:Ratio = new Ratio(BigInt(0),BigInt(1))
    var tr_Ep:Ratio = new Ratio(BigInt(0),BigInt(1))
    var pid:SlotId = (0,ByteArrayWrapper(Array()))
    var i = prefix+1
    breakable{
      for (id<-tine.ordered) {
        if (!id._2.data.isEmpty) {
          pid = getParentId(id) match {case value:SlotId => value}
          break()
        }
      }
      isValid &&= false
    }

    for (id <- tine.ordered) {
      if (isValid) updateLocalState(ls,Chain(id)) match {
        case value:State => {
          ls = value
        }
        case _ => {
          isValid &&= false
          println("Error: encountered invalid ledger in tine")
        }
      }
      if (isValid) getBlockHeader(id) match {
        case b:BlockHeader => {
          getParentBlockHeader(b) match {
            case pb:BlockHeader => {
              isValid &&= getParentId(b) == pid
              if (isValid) {
                compareBlocks(pb,b)
                pid = id
              }
            }
            case _ => {
              println("Error: parent id mismatch in tine")
              isValid &&= false
            }
          }
        }
        case _ =>
      }
      if (isValid) history.add(id._2,ls,eta_Ep)
    }

    def compareBlocks(parent:BlockHeader, block:BlockHeader) = {
      val (h0, _, slot, cert, rho, pi, _, pk_kes,bn,ps) = block
      val (pk_vrf, y, pi_y, pk_sig, tr_c,info) = cert
      while(i<=slot) {
        if (i/epochLength > ep) {
          ep = i/epochLength
          if (ep0 + 1 == ep) {
            eta_Ep = eta(subChain(localChain, 0, prefix) ++ tine, ep, eta_Ep)
            stakingState = {
              val eps = (ep - 1) * epochLength
              history.get(localChain.getLastActiveSlot(eps)._2) match {
                case value:(State,Eta) => {
                  value._1
                }
                case _ => {
                  isValid &&= false
                  Map()
                }
              }
            }
          } else {
            eta_Ep = eta(subChain(localChain, 0, prefix) ++ tine, ep, eta_Ep)
            updateLocalState(stakingState, subChain(subChain(localChain, 0, prefix) ++ tine, (i / epochLength) * epochLength - 2 * epochLength + 1, (i / epochLength) * epochLength - epochLength)) match {
              case value:State => stakingState = value
              case _ => println("Error: encountered invalid ledger in tine")
            }
          }
        }
        i+=1
      }
      alpha_Ep = relativeStake(ByteArrayWrapper(pk_sig++pk_vrf++pk_kes),stakingState)
      tr_Ep = phi(alpha_Ep)
      isValid &&= (
             hash(parent,serializer) == h0
          && verifyBlock(block)
          && parent._3 == ps
          && parent._9+1 == bn
          && vrf.vrfVerify(pk_vrf, eta_Ep ++ serializer.getBytes(slot) ++ serializer.getBytes("NONCE"), pi)
          && vrf.vrfProofToHash(pi).deep == rho.deep
          && vrf.vrfVerify(pk_vrf, eta_Ep ++ serializer.getBytes(slot) ++ serializer.getBytes("TEST"), pi_y)
          && vrf.vrfProofToHash(pi_y).deep == y.deep
          && tr_Ep == tr_c
          && compare(y, tr_Ep)
        )
      if(!isValid){
        print("Error: Holder "+holderIndex.toString+" ");print(slot);print(" ")
        println(Seq(
            hash(parent,serializer) == h0 //1
          , verifyBlock(block) //2
          , parent._3 == ps //3
          , parent._9+1 == bn //4
          , vrf.vrfVerify(pk_vrf,eta_Ep++serializer.getBytes(slot)++serializer.getBytes("NONCE"),pi) //5
          , vrf.vrfProofToHash(pi).deep == rho.deep //6
          , vrf.vrfVerify(pk_vrf,eta_Ep++serializer.getBytes(slot)++serializer.getBytes("TEST"),pi_y) //7
          , vrf.vrfProofToHash(pi_y).deep == y.deep //8
          , tr_Ep == tr_c //9
          , compare(y,tr_Ep) //10
        ))
        println("Holder "+holderIndex.toString+" Epoch:"+(slot/epochLength).toString+"\n"+"Eta:"+Base58.encode(eta_Ep))
        println(info)
      }
    }

    if(!isValid) SharedData.throwError(holderIndex)
    if (SharedData.error) {
      for (id<-(subChain(localChain,0,prefix)++tine).ordered) {
        if (id._1 > -1) println("H:"+holderIndex.toString+"S:"+id._1.toString+"ID:"+Base58.encode(id._2.data))
      }
    }
    isValid
  }

  /**
    * verify a signed issued transaction
    * @param t transaction
    * @return true if valid, false otherwise
    */
  def verifyTransaction(t:Transaction):Boolean = {
    t.verify(sig,serializer)
  }

  /**
    * apply each block in chain to passed local state
    * @param ls old local state to be updated
    * @param c chain of block ids
    * @return updated localstate
    */
  def updateLocalState(ls:State, c:Chain): Any = {
    var nls:State = ls
    var isValid = true
    for (id <- c.ordered) {
      if (isValid) getBlockHeader(id) match {
        case b:BlockHeader => {
          val (_,ledger:Ledger,slot:Slot,cert:Cert,_,_,_,pk_kes:PublicKey,_,_) = b
          val (pk_vrf,_,_,pk_sig,_,_) = cert
          val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
          var validForger = true
          if (slot == 0) {
            for (entry <- ledger) {
              entry match {
                case box:Box => {
                  if (verifyBox(box)) {
                    box.data match {
                      case entry:(ByteArrayWrapper,PublicKeyW,BigInt) => {
                        if (entry._1 == genesisBytes) {
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
                      case _ => isValid = false
                    }
                  }
                }
                case _ =>
              }
            }
          } else {
            ledger.head match {
              case box:Box => {
                if (verifyBox(box)) {
                  box.data match {
                    case entry:(ByteArrayWrapper,BigInt) => {
                      val delta = entry._2
                      if (entry._1 == forgeBytes && delta == BigDecimal(forgerReward).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt) {
                        if (nls.keySet.contains(pk_f)) {
                          val netStake: BigInt = nls(pk_f)._1
                          val txC:Int = nls(pk_f)._3
                          val newStake: BigInt = netStake + BigDecimal(forgerReward).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
                          nls -= pk_f
                          nls += (pk_f -> (newStake,true,txC))
                        } else {
                          validForger = false
                        }
                      } else {
                        validForger = false
                      }
                    }
                    case _ => validForger = false
                  }
                } else {
                  validForger = false
                }
              }
              case _ => validForger = false
            }
            if (validForger) {
              for (entry <- ledger.tail) {
                entry match {
                  case trans:Transaction => {
                    if (verifyTransaction(trans)) {
                      trans.applyTransaction(nls,pk_f,fee_r) match {
                        case value:State => {
                          nls = value
                        }
                        case _ => isValid = false
                      }
                    } else {
                      isValid = false
                    }
                  }
                  case _ => isValid = false
                }
              }
            } else {
              isValid = false
            }
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
      nls
    } else {
      0
    }
  }

  def trimMemPool: Unit = {
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
  def collectLedger(c:Chain): Unit = {
    for (id <- c.ordered) {
      getBlockHeader(id) match {
        case b:BlockHeader => {
          val ledger:Ledger = b._2
          for (entry <- ledger.tail) {
            entry match {
              case trans:Transaction => {
                if (!memPool.keySet.contains(trans.sid)) {
                  if (verifyTransaction(trans)) memPool += (trans.sid->(trans,0))
                }
              }
              case _ =>
            }
          }
        }
        case _ =>
      }
    }
  }

  /**
    * sorts buffer and adds transaction to ledger during block forging
    * @param pkw public key triad of forger
    * @return list of transactions
    */
  def chooseLedger(pkw:PublicKeyW,mp:MemPool,s:State): Ledger = {
    var ledger: Ledger = List()
    var ls: State = s
    val sortedBuffer = ListMap(mp.toSeq.sortWith(_._2._1.nonce < _._2._1.nonce): _*)
    breakable {
      for (entry <- sortedBuffer) {
        val transaction:Transaction = entry._2._1
        val transactionCount:Int = transaction.nonce
        if (transactionCount == ls(transaction.sender)._3 && verifyTransaction(transaction)) {
          transaction.applyTransaction(ls, pkw,fee_r) match {
            case value:State => {
              ledger ::= entry._2._1
              ls = value
            }
            case _ =>
          }
          if (ledger.length >= txPerBlock) break
        }
      }
    }
    ledger.reverse
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

  /**
    * utility for timing execution of methods
    * @param block any execution block
    * @tparam R
    * @return
    */
  def timeFlag[R](block: => R): R = {
    if (timingFlag && holderIndex == 0) {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()
      val outTime = (t1 - t0)*1.0e-9
      val tString = "%6.6f".format(outTime)
      println("Elapsed time: " + tString + " s")
      result
    } else {
      block
    }
  }

  def time[R](block: => R): R = {
     {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()
      val outTime = (t1 - t0)*1.0e-9
      val tString = "%6.6f".format(outTime)
      println("Elapsed time: " + tString + " s")
      result
    }
  }
}
