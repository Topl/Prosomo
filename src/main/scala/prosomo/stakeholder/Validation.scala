package prosomo.stakeholder

import com.google.common.primitives.Ints
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Block, Tine, Transaction}
import prosomo.primitives.{Mac, Parameters, Ratio, SharedData, Types}
import scorex.util.encode.Base58

import scala.math.BigInt
import scala.util.control.Breaks.{break, breakable}

/**
  * AMS 2020:
  * All verification methods required to participate in consensus
  */

trait Validation extends Members with Types {
  import Parameters._
  /**
    * Block verify using key evolving signature
    * @param b input block
    * @return true if signature is valid, false otherwise
    */
  def verifyBlockHeader(b:BlockHeader): Boolean = {
    val (hash, ledger, slot, cert, rho, pi, sig, pk_kes, bn,ps) = b
    kes.verify(sig._5, hash.data++serializer.getBytes(ledger)
        ++serializer.getBytes(slot)
        ++serializer.getBytes(cert)
        ++rho++pi++serializer.getBytes(bn)
        ++serializer.getBytes(ps),
      (sig._1,sig._2,sig._3), slot-sig._4) &&
      pk_kes.deep == fch.hash(Ints.toByteArray(sig._4)++sig._5).deep &&
      verifyMac(ledger.dataHash,ledger)
  }

  def verifyBlock(b:Block): Boolean = {
    val header = b.prosomoHeader
    val headerVer = verifyBlockHeader(header)
    val ledgerVer = if (header._3 == 0) {
      b.genesisSet match {
        case Some(txs:GenesisSet) =>
          if (txs.nonEmpty) {
            hashGen(txs,serializer) == header._2.dataHash && txs.map {
              input: (Array[Byte], ByteArrayWrapper, BigInt, Mac) =>
              verifyMac(hashGenEntry((input._1, input._2, input._3), serializer), input._4)
            }.reduceLeft(_ && _)
          } else {
            false
          }
        case _ => println("Error: tx set match in block verify"); false
      }
    } else {
      b.blockBody match {
        case Some(txs:TransactionSet) =>
          if (txs.length <= txPerBlock){
            if (txs.nonEmpty) {
              val (out1,out2) = (hash(txs,serializer) == header._2.dataHash , txs.map(verifyTransaction).reduceLeft(_ && _))
              if (!out1) println("Error: txs hash failed")
              if (!out2) println("Error: txs verify failed")
              out1 && out2
            } else {
              val out = hash(txs,serializer) == header._2.dataHash
              if (!out) println("Error: empty txs failed hash")
              out
            }
          } else {
            println("Error: txs length greater than tx/block")
            false
          }
        case _ => println("Error: tx set match in block verify"); false
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
    var eta_Ep: Eta = eta_from_genesis(c, 0)
    var staking_state_tine: State = Map()
    var pid:SlotId = (0,gh)
    var i = 0

    getBlockHeader(c.get(0)) match {
      case Some(b:BlockHeader) => bool &&= hash(b,serializer) == gh
      case _ => bool &&= false
    }
    if (!bool) println("Holder "+holderIndex.toString+" invalid genesis block")

    for (id <- c.ordered.tail) {
      getBlockHeader(id) match {
        case Some(b:BlockHeader) =>
          getParentBlockHeader(b) match {
            case Some(pb:BlockHeader) =>
              bool &&= getParentId(b) == pid
              if (getParentId(b) != pid) {
                println("Holder "+holderIndex.toString+" pid mismatch")
                println(s"bs:${b._3} pbs:${pid._1}")
              }
              compareBlocks(pb,b)
              pid = id
            case _ => bool &&= false
          }
        case _ =>
      }
    }

    def compareBlocks(parent: BlockHeader, block: BlockHeader): Unit = {
      val (h0, _, slot, cert, rho, pi, _, pk_kes, bn, ps) = block
      val (pk_vrf, y, pi_y, pk_sig, tr_c,_) = cert
      while(i<=slot) {
        if (i/epochLength > ep) {
          ep = i/epochLength
          eta_Ep = eta_from_tine(c, ep, eta_Ep)
          val toUpdate:State = if(ep == 0 || ep == 1) {Map()} else staking_state_tine
          val epochChain =
            subChain(c,(i/epochLength)*epochLength-2*epochLength+1,(i/epochLength)*epochLength-epochLength)
          updateLocalState(toUpdate,epochChain) match {
            case Some(value:State) =>  staking_state_tine = value
            case _ =>
              println("Error: encountered invalid ledger in local chain")
              bool &&= false
          }
        }
        i+=1
      }
      alpha_Ep = relativeStake(ByteArrayWrapper(pk_sig++pk_vrf++pk_kes), staking_state_tine)
      if (f_dynamic) {
        tr_Ep = threshold(alpha_Ep,slot-ps)
      } else {
        tr_Ep = phi(alpha_Ep)
      }
      val test:Rho = testStrategy match {
        case "vrf" => y
        case "parent-slot-hash" => Sha512(y++serializer.getBytes(ps))
        case "parent-slot-number-hash" => Sha512(y++serializer.getBytes(ps)++serializer.getBytes(bn))
      }

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
          && compare(test, tr_Ep)
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
          , compare(test, tr_Ep)//10
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
    var gcCounter = 0
    val candidateTine = subChain(localChain, 0, prefix) ++ tine

    history.get(candidateTine.getLastActiveSlot(prefix)) match {
      case Some(value:(State,Eta)) =>
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
            gcCounter += 1
            if (gcCounter%100==0) System.gc()
            updateLocalState(ls,id) match {
              case Some(newState:State) =>
                getBlockHeader(id) match {
                  case Some(block:BlockHeader) =>
                    getParentBlockHeader(block) match {
                      case Some(parent:BlockHeader) =>
                        if (getParentId(block) == pid) {
                          val (h0, _, slot, cert, rho, pi, _, pk_kes,bn,ps) = block
                          val (pk_vrf, y, pi_y, pk_sig, tr_c,info) = cert
                          while(currentSlot<=slot) {
                            updateEpoch(currentSlot,ep,eta_tine,candidateTine) match {
                              case result:(Int,Eta) if result._1 > ep =>
                                ep = result._1
                                eta_tine = result._2
                                staking_state_tine = getStakingState(ep,candidateTine)
                              case _ =>
                            }
                            currentSlot+=1
                          }
                          alpha_Ep = relativeStake(ByteArrayWrapper(pk_sig++pk_vrf++pk_kes),staking_state_tine)
                          if (f_dynamic) {
                            tr_Ep = threshold(alpha_Ep,slot-ps)
                          } else {
                            tr_Ep = phi(alpha_Ep)
                          }
                          val test = testStrategy match {
                            case "vrf" => y
                            case "parent-slot-hash" => Sha512(y++serializer.getBytes(ps))
                            case "parent-slot-number-hash" => Sha512(y++serializer.getBytes(ps)++serializer
                              .getBytes(bn))
                          }
                          isValid &&= (
                            hash(parent,serializer) == h0
                              && verifyBlockHeader(block)
                              && parent._3 == ps
                              && parent._9+1 == bn
                              && vrf.vrfVerify(pk_vrf, eta_tine ++ serializer.getBytes(slot) ++ serializer
                              .getBytes("NONCE"), pi)
                              && vrf.vrfProofToHash(pi).deep == rho.deep
                              && vrf.vrfVerify(pk_vrf, eta_tine ++ serializer.getBytes(slot) ++ serializer
                              .getBytes("TEST"), pi_y)
                              && vrf.vrfProofToHash(pi_y).deep == y.deep
                              && tr_Ep == tr_c
                              && compare(test, tr_Ep)
                            )

                          if (isValid) {
                            history.add(id,newState,eta_tine)
                            ls = newState
                            pid = id
                          } else {
                            print("Error: Holder "+holderIndex.toString+s" invalid block, id = ${Base58.encode(id._2.data)}")
                            println(Seq(
                              hash(parent,serializer) == h0 //1
                              , verifyBlockHeader(block) //2
                              , parent._3 == ps //3
                              , parent._9+1 == bn //4
                              , vrf.vrfVerify(pk_vrf,eta_tine++serializer.getBytes(slot)++serializer
                                .getBytes("NONCE"),pi) //5
                              , vrf.vrfProofToHash(pi).deep == rho.deep //6
                              , vrf.vrfVerify(pk_vrf,eta_tine++serializer.getBytes(slot)++serializer
                                .getBytes("TEST"),pi_y) //7
                              , vrf.vrfProofToHash(pi_y).deep == y.deep //8
                              , tr_Ep == tr_c //9
                              , compare(test, tr_Ep) //10
                            ))
                            println(s"Holder $holderIndex, ep: $ep, eta_tine: ${Base58.encode(eta_tine)}")
                            println(info)
                            break()
                          }
                        } else {
                          println("Error: parent id mismatch")
                          println(s"pid ${Base58.encode(pid._2.data)}")
                          println(s"id ${Base58.encode(id._2.data)}")
                          isValid &&= false
                          break()
                        }
                      case _ =>
                        println("Error: could not recover parent header")
                        println("block id:"+Base58.encode(id._2.data))
                        println("parentId:"+Base58.encode(block._1.data))
                        isValid &&= false
                        break()
                    }
                  case _ =>
                    println("Error: encountered invalid header in tine")
                    isValid &&= false
                    break()
                }
              case _ =>
                println("Error: encountered invalid ledger in tine")
                isValid &&= false
                break()
            }
          }
        }
      case _ =>
        println("Error: could not recover prefix state")
        isValid &&= false
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
