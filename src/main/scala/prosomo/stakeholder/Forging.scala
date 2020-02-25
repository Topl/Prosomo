package prosomo.stakeholder

import akka.actor.ActorRef
import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.SendBlock
import prosomo.components.{Block, Chain}
import prosomo.primitives.Parameters.{genesisBytes, initStakeMax, initStakeMin, printFlag, stakeDistribution, stakeScale}
import prosomo.primitives.{Box, Keys, MalkinKey, Ratio, SharedData}
import scorex.crypto.encode.Base58

import scala.math.BigInt

trait Forging extends Members {

  /**determines eligibility for a stakeholder to be a slot leader then calculates a block with epoch variables */
  def forgeBlock(forgerKeys:Keys) = {
    def blockInfo:String = {
      "forger_index:"+holderIndex.toString+",adversarial:"+adversary.toString+",eta:"+Base58.encode(eta)+",epoch:"+currentEpoch.toString
    }
    val slot = localSlot
    val pi_y: Pi = vrf.vrfProof(forgerKeys.sk_vrf, eta ++ serializer.getBytes(slot) ++ serializer.getBytes("TEST"))
    val y: Rho = vrf.vrfProofToHash(pi_y)
    if (compare(y, forgerKeys.threshold)) {
      //println("Eta on forging:"+Base58.encode(eta))
      roundBlock = {
        val pb:BlockHeader = getBlockHeader(localChain.getLastActiveSlot(localSlot-1)) match {case b:BlockHeader => b}
        val bn:Int = pb._9 + 1
        val ps:Slot = pb._3
        val txs:TransactionSet = chooseLedger(forgerKeys.pkw,memPool,localState)
        val pi: Pi = vrf.vrfProof(forgerKeys.sk_vrf, eta ++ serializer.getBytes(slot) ++ serializer.getBytes("NONCE"))
        val rho: Rho = vrf.vrfProofToHash(pi)
        val h: Hash = hash(pb,serializer)
        val ledger:Box = signBox(hash(txs,serializer), sessionId, forgerKeys.sk_sig, forgerKeys.pk_sig)
        val cert: Cert = (forgerKeys.pk_vrf, y, pi_y, forgerKeys.pk_sig, forgerKeys.threshold,blockInfo)
        val kes_sig: KesSignature = forgerKeys.sk_kes.sign(kes,h.data++serializer.getBytes(ledger)++serializer.getBytes(slot)++serializer.getBytes(cert)++rho++pi++serializer.getBytes(bn)++serializer.getBytes(ps))
        val b = (h, ledger, slot, cert, rho, pi, kes_sig, forgerKeys.pk_kes,bn,ps)
        val hb = hash(b,serializer)
        if (printFlag) {println("Holder " + holderIndex.toString + s" forged block $bn with id:"+Base58.encode(hb.data))}
        val block = new Block(hb,b,txs)
        blocks.add(block,serializer)
        assert(localChain.getLastActiveSlot(localSlot)._2 == b._1)
        localChain.update((localSlot, hb))
        chainHistory.update((localSlot,hb),serializer)
        send(self,gossipers, SendBlock(block,signBox(block.id, sessionId, keys.sk_sig, keys.pk_sig)))
        blocksForged += 1
        updateLocalState(localState, Chain(localChain.get(localSlot))) match {
          case value:State => localState = value
          case _ => {
            SharedData.throwError(holderIndex)
            println("error: invalid ledger in forged block")
          }
        }
        history.add(hb,localState,eta,serializer)
        updateWallet
        trimMemPool
        validateChainIds(localChain)
        b
      }
    } else {
      roundBlock = -1
    }
  }

  def forgeGenBlock(eta0:Eta,
                    genKeys:Map[String,String],
                    coordId:String,
                    pk_sig:PublicKey,
                    pk_vrf:PublicKey,
                    pk_kes:PublicKey,
                    sk_sig:PublicKey,
                    sk_vrf:PublicKey,
                    sk_kes:MalkinKey
                   ): (Block,Map[ActorRef,PublicKeyW]) = {
    val bn:Int = 0
    val ps:Slot = -1
    val slot:Slot = 0
    val pi:Pi = vrf.vrfProof(sk_vrf,eta0++serializer.getBytes(slot)++serializer.getBytes("NONCE"))
    val rho:Rho = vrf.vrfProofToHash(pi)
    val pi_y:Pi = vrf.vrfProof(sk_vrf,eta0++serializer.getBytes(slot)++serializer.getBytes("TEST"))
    val y:Rho = vrf.vrfProofToHash(pi_y)
    val h:Hash = ByteArrayWrapper(eta0)
    var holderKeys:Map[ActorRef,PublicKeyW] = Map()
    val genesisEntries: GenesisSet = holders.map{
      case ref:ActorRef => {
        val initStake = {
          val out = stakeDistribution match {
            case "random" => {initStakeMax*rng.nextDouble}
            case "exp" => {initStakeMax*math.exp(-stakeScale*holders.indexOf(ref).toDouble)}
            case "flat" => {initStakeMax}
          }
          if (initStakeMax > initStakeMin && out > initStakeMin) {
            out
          } else {
            if (initStakeMin > 1.0) {
              initStakeMin
            } else {
              1.0
            }
          }
        }
        val pkw = ByteArrayWrapper(hex2bytes(genKeys(s"${ref.path}").split(";")(0))++hex2bytes(genKeys(s"${ref.path}").split(";")(1))++hex2bytes(genKeys(s"${ref.path}").split(";")(2)))
        holderKeys += (ref-> pkw)
        (genesisBytes.data, pkw, BigDecimal(initStake).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt,signBox(hashGenEntry((genesisBytes.data, pkw, BigDecimal(initStake).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt), serializer), ByteArrayWrapper(FastCryptographicHash(coordId)),sk_sig,pk_sig))
      }
    }
    val ledger:Box = signBox(hashGen(genesisEntries,serializer), ByteArrayWrapper(FastCryptographicHash(coordId)),sk_sig,pk_sig)
    val cert:Cert = (pk_vrf,y,pi_y,pk_sig,new Ratio(BigInt(1),BigInt(1)),"genesis")
    val sig:KesSignature = sk_kes.sign(kes, h.data++serializer.getBytes(ledger)++serializer.getBytes(slot)++serializer.getBytes(cert)++rho++pi++serializer.getBytes(bn)++serializer.getBytes(ps))
    val genesisHeader:BlockHeader = (h,ledger,slot,cert,rho,pi,sig,pk_kes,bn,ps)
    (new Block(hash(genesisHeader,serializer),genesisHeader,genesisEntries),holderKeys)
  }

}
