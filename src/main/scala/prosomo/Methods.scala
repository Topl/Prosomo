package prosomo

import akka.actor.ActorRef
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.language.postfixOps
import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.immutable.ListMap
import util.control.Breaks._
import scala.math.BigInt
import scala.util.Random
import scorex.crypto.encode.Base58


trait Methods
  extends Functions {

  //vars for chain, blocks, state, history, and locks
  var localChain:Chain = Array()
  var blocks:ChainData = Array()
  var chainHistory:ChainHistory = Array()
  var localState:State = Map()
  var eta:Eta = Array()
  var stakingState:State = Map()
  var memPool:MemPool = Map()
  var holderIndex:Int = -1
  var diffuseSent = false

  //verification and signing objects
  val vrf = new Vrf
  val kes = new Kes
  val sig = new Sig

  val history:History = new History
  //val mempool:Mempool = new Mempool
  var rng:Random = new Random
  var routerRef:ActorRef = _

  /**
    * retrieve a block from database
    * @param bid
    * @return block if found, 0 otherwise
    */
  def getBlock(bid:BlockId): Any = {
    if (bid._1 >= 0 && !bid._2.data.isEmpty) {
      if (blocks(bid._1).contains(bid._2)) {
        blocks(bid._1)(bid._2)
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
  def getParentBlock(b:Block): Any = {
    if (b._10 >= 0 && !b._1.data.isEmpty) {
      if (blocks(b._10).contains(b._1)) {
        blocks(b._10)(b._1)
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
  def getParentId(bid:BlockId): Any = {
    getBlock(bid) match {
      case b:Block => (b._10,b._1)
      case _ => 0
    }
  }

  /**
    * calculates epoch nonce recursively
    * @param c local chain to be verified
    * @param ep epoch derived from time step
    * @return hash nonce
    */
  def eta(c:Chain,ep:Int): Eta = {
    if(ep == 0) {
      getBlock(c(0)) match {
        case b:Block => b._1.data
        case _ => Array()
      }
    } else {
      var v: Array[Byte] = Array()
      val epcv = subChain(c,ep*epochLength-epochLength,ep*epochLength-epochLength/3)
      val cnext = subChain(c,0,ep*epochLength-epochLength)
      for(id <- epcv) {
        getBlock(id) match {
          case b:Block => v = v++b._5
          case _ =>
        }
      }
      FastCryptographicHash(eta(cnext,ep-1)++serialize(ep)++v)
    }
  }

  /**
    * calculates epoch nonce from previous nonce
    * @param c local chain to be verified
    * @param ep epoch derived from time step
    * @param etaP previous eta
    * @return hash nonce
    */
  def eta(c:Chain,ep:Int,etaP:Eta): Eta = {
    if(ep == 0) {
      getBlock(c(0)) match {
        case b:Block => b._1.data
        case _ => Array()
      }
    } else {
      var v: Array[Byte] = Array()
      val epcv = subChain(c,ep*epochLength-epochLength,ep*epochLength-epochLength/3)
      for(id <- epcv) {
        getBlock(id) match {
          case b:Block => v = v++b._5
          case _ =>
        }
      }
      val eta_ep = FastCryptographicHash(etaP++serialize(ep)++v)
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
    str+";"+id+";"+bytes2hex(sig.sign(sk_sig,serialize(str+";"+id)))
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
    (data,id,sig.sign(sk_sig,serialize(data)++id.data),pk_sig)
  }

  /**
    * verify a
    * @param box
    * @return
    */
  def verifyBox(box:Box): Boolean = {
    sig.verify(box._3,serialize(box._1)++box._2.data,box._4)
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
          case t:ChainData => blocks = t
          case _ => println("error")
        }
        value.h match {
          case h:ChainHistory => chainHistory = h
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
  def verifyBlock(b:Block): Boolean = {
    val (hash, ledger, slot, cert, rho, pi, sig, pk_kes, bn,ps) = b
    val kesVer = kes.verify(pk_kes,hash.data++serialize(ledger)++serialize(slot)++serialize(cert)++rho++pi++serialize(bn)++serialize(ps),sig,slot)
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
    var alpha_Ep = 0.0
    var tr_Ep = 0.0
    var eta_Ep: Eta = eta(c, 0)
    var stakingState: State = Map()
    var pid:BlockId = (0,gh)
    var i = 0

    getBlock(c(0)) match {
      case b:Block => bool &&= hash(b) == gh
      case _ => bool &&= false
    }

    for (id <- c.tail) {
      getBlock(id) match {
        case b:Block => {
          getParentBlock(b) match {
            case pb:Block => {
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

    def compareBlocks(parent: Block, block: Block) = {
      val (h0, _, slot, cert, rho, pi, _, pk_kes, bn, ps) = block
      val (pk_vrf, y, pi_y, pk_sig, tr_c,_) = cert
      while(i<=slot) {
        if (i/epochLength > ep) {
          ep = i/epochLength
          eta_Ep = eta(c, ep, eta_Ep)
          updateLocalState(stakingState,subChain(c,(i/epochLength)*epochLength-2*epochLength+1,(i/epochLength)*epochLength-epochLength)) match {
            case value:State =>  stakingState = value
            case _ => println("Error: encountered invalid ledger in local chain")
          }
        }
        i+=1
      }
      alpha_Ep = relativeStake(ByteArrayWrapper(pk_sig++pk_vrf++pk_kes), stakingState)
      tr_Ep = phi(alpha_Ep, f_s)
      bool &&= (
        hash(parent) == h0
          && verifyBlock(block)
          && parent._3 == ps
          && parent._9 + 1 == bn
          && vrf.vrfVerify(pk_vrf, eta_Ep ++ serialize(slot) ++ serialize("NONCE"), pi)
          && vrf.vrfProofToHash(pi).deep == rho.deep
          && vrf.vrfVerify(pk_vrf, eta_Ep ++ serialize(slot) ++ serialize("TEST"), pi_y)
          && vrf.vrfProofToHash(pi_y).deep == y.deep
          && tr_Ep == tr_c
          && compare(y, tr_Ep)
        )
      if (!bool) {
        print(slot)
        print(" ")
        println(Seq(
          hash(parent) == h0 //1
          , verifyBlock(block) //2
          , parent._3 == ps //3
          , parent._9 + 1 == bn //4
          , vrf.vrfVerify(pk_vrf, eta_Ep ++ serialize(slot) ++ serialize("NONCE"), pi) //5
          , vrf.vrfProofToHash(pi).deep == rho.deep //6
          , vrf.vrfVerify(pk_vrf, eta_Ep ++ serialize(slot) ++ serialize("TEST"), pi_y) //7
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
  def verifySubChain(tine:Chain,prefix:Slot): Boolean = {
    var isValid = true
    val ep0 = prefix/epochLength
    var eta_Ep:Eta = Array()
    var ls:State = Map()

    history.get(localChain(prefix)._2) match {
      case value:(State,Eta) => {
        ls = value._1
        eta_Ep = value._2
      }
      case _ => isValid &&= false
    }


    var stakingState: State = {
      if (ep0 > 1) {
        history.get(localChain((ep0-1)*epochLength)._2) match {
          case value:(State,Eta) => {
            value._1
          }
          case _ => {
            isValid &&= false
            Map()
          }
        }
      } else {
        history.get(localChain(0)._2) match {
          case value:(State,Eta) => {
            value._1
          }
          case _ => {
            isValid &&= false
            Map()
          }
        }
      }
    }

    var ep = ep0
    var alpha_Ep = 0.0
    var tr_Ep = 0.0
    var pid:BlockId = (0,ByteArrayWrapper(Array()))
    var i = prefix+1
    breakable{
      for (id<-tine) {
        if (!id._2.data.isEmpty) {
          pid = getParentId(id) match {case value:BlockId => value}
          break()
        }
      }
      isValid &&= false
    }

    for (id <- tine) {
      if (isValid) updateLocalState(ls,Array(id)) match {
        case value:State => {
          ls = value
        }
        case _ => {
          isValid &&= false
          println("Error: encountered invalid ledger in tine")
        }
      }
      if (isValid) getBlock(id) match {
        case b:Block => {
          getParentBlock(b) match {
            case pb:Block => {
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

    def compareBlocks(parent:Block,block:Block) = {
      val (h0, _, slot, cert, rho, pi, _, pk_kes,bn,ps) = block
      val (pk_vrf, y, pi_y, pk_sig, tr_c,info) = cert
      while(i<=slot) {
        if (i/epochLength > ep) {
          ep = i/epochLength
          if (ep0 + 1 == ep) {
            eta_Ep = eta(subChain(localChain, 0, prefix) ++ tine, ep, eta_Ep)
            stakingState = {
              history.get(localChain((ep - 1) * epochLength)._2) match {
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
      tr_Ep = phi(alpha_Ep, f_s)
      isValid &&= (
             hash(parent) == h0
          && verifyBlock(block)
          && parent._3 == ps
          && parent._9+1 == bn
          && vrf.vrfVerify(pk_vrf, eta_Ep ++ serialize(slot) ++ serialize("NONCE"), pi)
          && vrf.vrfProofToHash(pi).deep == rho.deep
          && vrf.vrfVerify(pk_vrf, eta_Ep ++ serialize(slot) ++ serialize("TEST"), pi_y)
          && vrf.vrfProofToHash(pi_y).deep == y.deep
          && tr_Ep == tr_c
          && compare(y, tr_Ep)
        )
      if(!isValid){
        print("Error: Holder "+holderIndex.toString+" ");print(slot);print(" ")
        println(Seq(
            hash(parent) == h0 //1
          , verifyBlock(block) //2
          , parent._3 == ps //3
          , parent._9+1 == bn //4
          , vrf.vrfVerify(pk_vrf,eta_Ep++serialize(slot)++serialize("NONCE"),pi) //5
          , vrf.vrfProofToHash(pi).deep == rho.deep //6
          , vrf.vrfVerify(pk_vrf,eta_Ep++serialize(slot)++serialize("TEST"),pi_y) //7
          , vrf.vrfProofToHash(pi_y).deep == y.deep //8
          , tr_Ep == tr_c //9
          , compare(y,tr_Ep) //10
        ))
        println("Holder "+holderIndex.toString+" Epoch:"+(slot/epochLength).toString+"\n"+"Eta:"+Base58.encode(eta_Ep))
        println(info)
      }
    }

    if(!isValid) sharedData.throwError
    if (sharedData.error) {
      for (id<-subChain(localChain,0,prefix)++tine) {
        if (id._1 > -1) println("H:"+holderIndex.toString+"S:"+id._1.toString+"ID:"+Base58.encode(id._2.data))
      }
    }
    if (isValid) {
      localState = ls
      eta = eta_Ep
    }
    isValid
  }

  /**
    * verify a signed issued transaction
    * @param t transaction
    * @return true if valid, false otherwise
    */
  def verifyTransaction(t:Transaction):Boolean = {
    sig.verify(t._6,t._2.data++t._3.toByteArray++t._4.data++serialize(t._5),t._1.data.take(sig.KeyLength))
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
    for (id <- c) {
      getBlock(id) match {
        case b:Block => {
          val (_,ledger:Ledger,slot:Slot,cert:Cert,_,_,_,pk_kes:PublicKey,_,_) = b
          val (pk_vrf,_,_,pk_sig,_,_) = cert
          val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
          var validForger = true
          if (slot == 0) {
            for (entry <- ledger) {
              entry match {
                case box:Box => {
                  if (verifyBox(box)) {
                    box._1 match {
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
                  box._1 match {
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
                      applyTransaction(nls,trans,pk_f) match {
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
      if (entry._2._1._5 < localState(entry._2._1._1)._3) {
        memPool -= entry._1
      }
    }
  }

  /**
    * collects all transaction on the ledger of each block in the passed chain and adds them to the buffer
    * @param c chain to collect transactions
    */
  def collectLedger(c:Chain): Unit = {
    for (id <- c) {
      getBlock(id) match {
        case b:Block => {
          val ledger:Ledger = b._2
          for (entry <- ledger.tail) {
            entry match {
              case trans:Transaction => {
                if (!memPool.keySet.contains(trans._4)) {
                  if (verifyTransaction(trans)) memPool += (trans._4->(trans,0))
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
    val sortedBuffer = ListMap(mp.toSeq.sortWith(_._2._1._5 < _._2._1._5): _*)
    breakable {
      for (entry <- sortedBuffer) {
        val transaction:Transaction = entry._2._1
        val transactionCount:Int = transaction._5
        if (transactionCount == ls(transaction._1)._3 && verifyTransaction(transaction)) {
          applyTransaction(ls, transaction, pkw) match {
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
    sig.verify(hex2bytes(values(4)), serialize(m), hex2bytes(values(0)))
  }

  /**
    * utility for timing execution of methods
    * @param block any execution block
    * @tparam R
    * @return
    */
  def time[R](block: => R): R = {
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
}
