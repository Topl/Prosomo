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
              val (out1,out2) = (hash(txs,serializer) == header._2.dataHash , txs.map(verifyTransaction).reduceLeft(_ && _))
              if (!out1) println("error: txs hash failed")
              if (!out2) println("error: txs verify failed")
              out1 && out2
            } else {
              val out = hash(txs,serializer) == header._2.dataHash
              if (!out) println("error: empty txs failed hash")
              out
            }
          } else {
            println("error: txs length greater than tx/block")
            false
          }
        }
        case _ => {println("error: tx set match in block verify");false}
      }
    }
    val out = headerVer && b.id == hash(header,serializer) && ledgerVer
    if (!out) println(headerVer, b.id == hash(header,serializer) , ledgerVer)
    out
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
    var eta_Ep: Eta = eta_from_genesis(c, 0)
    var staking_state_tine: State = Map()
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
          eta_Ep = eta_from_tine(c, ep, eta_Ep)
          val toUpdate:State = if(ep == 0 || ep == 1) {Map()} else {staking_state_tine}
          val epochChain = subChain(c,(i/epochLength)*epochLength-2*epochLength+1,(i/epochLength)*epochLength-epochLength)
          updateLocalState(toUpdate,epochChain) match {
            case value:State =>  staking_state_tine = value
            case _ => {
              println("Error: encountered invalid ledger in local chain")
              bool &&= false
            }
          }
        }
        i+=1
      }
      alpha_Ep = relativeStake(ByteArrayWrapper(pk_sig++pk_vrf++pk_kes), staking_state_tine)
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

    val candidateTine = subChain(localChain, 0, prefix) ++ tine

    history.get(candidateTine.getLastActiveSlot(prefix)) match {
      case value:(State,Eta) => {
        val ep_prefix = prefix/epochLength
        val eta_prefix = value._2
        val ls_prefix = value._1
        var ep = ep_prefix
        var eta_tine:Eta = eta_prefix
        var ls:State = ls_prefix
        var staking_state_tine: State = getStakingState(ep_prefix,candidateTine)
        var alpha_Ep:Ratio = new Ratio(BigInt(0),BigInt(1))
        var tr_Ep:Ratio = new Ratio(BigInt(0),BigInt(1))
        var currentSlot = prefix+1
        var pid:SlotId = candidateTine.getLastActiveSlot(prefix)
        breakable{
          for (id <- tine.ordered) {
            updateLocalState(ls,id) match {
              case newState:State => {
                getBlockHeader(id) match {
                  case block:BlockHeader => {
                    getParentBlockHeader(block) match {
                      case parent:BlockHeader => {
                        if (getParentId(block) == pid) {
                          val (h0, _, slot, cert, rho, pi, _, pk_kes,bn,ps) = block
                          val (pk_vrf, y, pi_y, pk_sig, tr_c,info) = cert
                          while(currentSlot<=slot) {
                            updateEpoch(currentSlot,ep,eta_tine,candidateTine) match {
                              case result:(Int,Eta) if result._1 > ep => {
                                ep = result._1
                                eta_tine = result._2
                                staking_state_tine = getStakingState(ep,candidateTine)
                              }
                              case _ =>
                            }
                            currentSlot+=1
                          }
                          alpha_Ep = relativeStake(ByteArrayWrapper(pk_sig++pk_vrf++pk_kes),staking_state_tine)
                          tr_Ep = phi(alpha_Ep)
                          isValid &&= (
                            hash(parent,serializer) == h0
                              && verifyBlockHeader(block)
                              && parent._3 == ps
                              && parent._9+1 == bn
                              && vrf.vrfVerify(pk_vrf, eta_tine ++ serializer.getBytes(slot) ++ serializer.getBytes("NONCE"), pi)
                              && vrf.vrfProofToHash(pi).deep == rho.deep
                              && vrf.vrfVerify(pk_vrf, eta_tine ++ serializer.getBytes(slot) ++ serializer.getBytes("TEST"), pi_y)
                              && vrf.vrfProofToHash(pi_y).deep == y.deep
                              && tr_Ep == tr_c
                              && compare(y, tr_Ep)
                            )

                          if (isValid) {
                            history.add(id,newState,eta_tine)
                            ls = newState
                            pid = id
                          } else {
                            print("error: Holder "+holderIndex.toString+s" invalid block, id = ${Base58.encode(id._2.data)}")
                            println(Seq(
                              hash(parent,serializer) == h0 //1
                              , verifyBlockHeader(block) //2
                              , parent._3 == ps //3
                              , parent._9+1 == bn //4
                              , vrf.vrfVerify(pk_vrf,eta_tine++serializer.getBytes(slot)++serializer.getBytes("NONCE"),pi) //5
                              , vrf.vrfProofToHash(pi).deep == rho.deep //6
                              , vrf.vrfVerify(pk_vrf,eta_tine++serializer.getBytes(slot)++serializer.getBytes("TEST"),pi_y) //7
                              , vrf.vrfProofToHash(pi_y).deep == y.deep //8
                              , tr_Ep == tr_c //9
                              , compare(y,tr_Ep) //10
                            ))
                            println(s"Holder $holderIndex, ep: $ep, eta_tine: ${Base58.encode(eta_tine)}")
                            println(info)
                            break()
                          }
                        } else {
                          println("error: parent id mismatch")
                          println(s"pid ${Base58.encode(pid._2.data)}")
                          println(s"id ${Base58.encode(id._2.data)}")
                          isValid &&= false
                          break()
                        }
                      }
                      case _ => {
                        println("error: could not recover parent header")
                        println("block id:"+Base58.encode(id._2.data))
                        println("parentId:"+Base58.encode(block._1.data))
                        isValid &&= false
                        break()
                      }
                    }
                  }
                  case _ => {
                    println("error: encountered invalid header in tine")
                    isValid &&= false
                    break()
                  }
                }
              }
              case _ => {
                println("error: encountered invalid ledger in tine")
                isValid &&= false
                break()
              }
            }
          }
        }
      }
      case _ => {
        println("error: could not recover prefix state")
        isValid &&= false
      }
    }

    if(!isValid) SharedData.throwError(holderIndex)
    if (SharedData.error) {
      println(s"Prefix: $prefix")
      println(s"Epoch Prefix ${prefix/epochLength}")
      println("Local chain:")
      localChain.print
      println("Tine:")
      tine.print
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
