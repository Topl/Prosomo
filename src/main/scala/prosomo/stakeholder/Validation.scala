package prosomo.stakeholder

import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Block, Tine, Transaction}
import prosomo.primitives.{Mac, Parameters, Ratio, SharedData}
import scorex.crypto.encode.Base58

import scala.math.BigInt
import scala.util.control.Breaks.{break, breakable}

trait Validation extends Members {
  import Parameters._
  /**
    * Block verify using key evolving signature
    * @param b input block
    * @returnt true if signature is valid, false otherwise
    */
  def verifyBlockHeader(b:BlockHeader): Boolean = {
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
    verifyMac(ledger.dataHash,ledger) && kesVer
  }

  def verifyBlock(b:Block): Boolean = {
    val header = b.prosomoHeader
    val headerVer = verifyBlockHeader(header)
    val ledgerVer = if (header._3 == 0) {
      b.body match {
        case txs:GenesisSet => {
          if (txs.nonEmpty) {
            hashGen(txs,serializer) == header._2.dataHash && txs.map(
              _ match {case input:(Array[Byte], ByteArrayWrapper, BigInt,Mac) => {
                verifyMac(hashGenEntry((input._1,input._2,input._3),serializer),input._4)
              }}
            ).reduceLeft(_ && _)
          } else {
            false
          }
        }
        case _ => {println("error: tx set match in block verify");false}
      }
    } else {
      b.body match {
        case txs:TransactionSet => {
          if (txs.length <= txPerBlock){
            if (txs.nonEmpty) {
              hash(txs,serializer) == header._2.dataHash && txs.map(verifyTransaction).reduceLeft(_ && _)
            } else {
              hash(txs,serializer) == header._2.dataHash
            }
          } else {
            false
          }
        }
        case _ => {println("error: tx set match in block verify");false}
      }
    }
    headerVer && b.id == hash(header,serializer) && ledgerVer
  }

  /**
    * Verify chain using key evolving signature, VRF proofs, and hash id
    * @param c chain to be verified
    * @param gh genesis block hash
    * @return true if chain is valid, false otherwise
    */
  def verifyChain(c:Tine, gh:Hash): Boolean = {
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
    if (!bool) println("Holder "+holderIndex.toString+" invalid genesis block")

    for (id <- c.ordered.tail) {
      getBlockHeader(id) match {
        case b:BlockHeader => {
          getParentBlockHeader(b) match {
            case pb:BlockHeader => {
              bool &&= getParentId(b) == pid
              if (getParentId(b) != pid) {
                println("Holder "+holderIndex.toString+" pid mismatch")
                println(s"bs:${b._3} pbs:${pid._1}")
              }
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
          && verifyBlockHeader(block)
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
          , verifyBlockHeader(block) //2
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
  def verifySubChain(tine:Tine, prefix:Slot): Boolean = {
    var isValid = true
    val ep0 = prefix/epochLength
    var eta_Ep:Eta = Array()
    var ls:State = Map()

    history.get(localChain.getLastActiveSlot(prefix)) match {
      case value:(State,Eta) => {
        ls = value._1
        eta_Ep = value._2
      }
      case _ => isValid &&= false
    }

    var stakingState: State = {
      if (ep0 > 1) {
        history.get(localChain.getLastActiveSlot((ep0-1)*epochLength)) match {
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
        history.get(localChain.get(0)) match {
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
      if (isValid) updateLocalState(ls,Tine(id)) match {
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
      if (isValid) history.add(id,ls,eta_Ep)
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
              history.get(localChain.getLastActiveSlot(eps)) match {
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
          && verifyBlockHeader(block)
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
          , verifyBlockHeader(block) //2
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
    verifyTX(t,sig,serializer)
  }

}
