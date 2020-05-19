package prosomo.stakeholder

import prosomo.primitives.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.SendBlock
import prosomo.components.{Block, Tine}
import prosomo.primitives.Parameters._
import prosomo.primitives.{Keys, Mac, MalkinKey, Ratio, SharedData}
import scorex.util.encode.Base58

import scala.math.BigInt
import scala.util.Try

trait Forging extends Members {

  /**determines eligibility for a stakeholder to be a slot leader then calculates a block with epoch variables */
  def forgeBlock(forgerKeys:Keys):Unit = Try{
    val slot = globalSlot
    val pi_y: Pi = vrf.vrfProof(forgerKeys.sk_vrf, eta ++ serializer.getBytes(slot) ++ serializer.getBytes("TEST"))
    val y: Rho = vrf.vrfProofToHash(pi_y)
    val pb:BlockHeader = getBlockHeader(localChain.getLastActiveSlot(slot-1)).get
    assert(pb._3 != slot)
    val ps:Slot = pb._3
    if (f_dynamic) {
      forge(threshold(forgerKeys.alpha,slot,ps))
    } else {
      forge(phi(forgerKeys.alpha))
    }
    def forge(thr:Ratio) = if (compare(y, thr)) {
      def blockInfo:String = {
        "forger_index:"+holderIndex.toString+",adversarial:"+adversary.toString+",eta:"+Base58.encode(eta)+",epoch:"+currentEpoch.toString
      }
      val bn:Int = pb._9 + 1
      val txs:TransactionSet = chooseLedger(forgerKeys.pkw,memPool,localState)
      val pi: Pi = vrf.vrfProof(forgerKeys.sk_vrf, eta ++ serializer.getBytes(slot) ++ serializer.getBytes("NONCE"))
      val rho: Rho = vrf.vrfProofToHash(pi)
      val h: Hash = hash(pb,serializer)
      val ledger:Mac = signMac(hash(txs,serializer), sessionId, forgerKeys.sk_sig, forgerKeys.pk_sig)
      val cert: Cert = (forgerKeys.pk_vrf, y, pi_y, forgerKeys.pk_sig, thr,blockInfo)
      val kes_sig: KesSignature = forgerKeys.sk_kes.sign(kes,h.data++serializer.getBytes(ledger)++serializer.getBytes(slot)++serializer.getBytes(cert)++rho++pi++serializer.getBytes(bn)++serializer.getBytes(ps))
      val b = (h, ledger, slot, cert, rho, pi, kes_sig, forgerKeys.pk_kes,bn,ps)
      val hb = hash(b,serializer)
      if (printFlag) {println(s"Holder $holderIndex forged block $bn with id:${Base58.encode(hb.data)} with ${txs.length} txs")}
      val block = Block(hb,Some(b),Some(txs),None)
      blocks.add(block)
      updateLocalState(localState, (slot,block.id)) match {
        case Some(forgedState:State) => {
          assert(localChain.getLastActiveSlot(slot-1)._2 == b._1)
          send(ActorRefWrapper(self),gossipers, SendBlock(block,signMac(block.id, sessionId, keys.sk_sig, keys.pk_sig)))
          history.add((slot,block.id),forgedState,eta)
          blocksForged += 1
          val jobNumber = tineCounter
          tineCounter += 1
          candidateTines = candidateTines ++ Array((Tine((slot,block.id),rho),slot-1,jobNumber))
        }
        case _ => {
          SharedData.throwError(holderIndex)
          println("error: invalid ledger in forged block")
        }
      }
    }
  }

  def forgeGenBlock(eta0:Eta,
                    holderKeys:Map[Int,PublicKeyW],
                    coordId:String,
                    pk_sig:PublicKey,
                    pk_vrf:PublicKey,
                    pk_kes:PublicKey,
                    sk_sig:PublicKey,
                    sk_vrf:PublicKey,
                    sk_kes:MalkinKey
                   ): Block = {
    def genEntry(index:Int) = {
      val initStake = {
        val out = stakeDistribution match {
          case "random" => {initStakeMax*rng.nextDouble}
          case "exp" => {initStakeMax*math.exp(-stakeScale*index.toDouble)}
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
      val pkw = holderKeys(index)
      (genesisBytes.data, pkw, BigDecimal(initStake).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt,signMac(hashGenEntry((genesisBytes.data, pkw, BigDecimal(initStake).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt), serializer), ByteArrayWrapper(FastCryptographicHash(coordId)),sk_sig,pk_sig))
    }
    val bn:Int = 0
    val ps:Slot = -1
    val slot:Slot = 0
    val pi:Pi = vrf.vrfProof(sk_vrf,eta0++serializer.getBytes(slot)++serializer.getBytes("NONCE"))
    val rho:Rho = vrf.vrfProofToHash(pi)
    val pi_y:Pi = vrf.vrfProof(sk_vrf,eta0++serializer.getBytes(slot)++serializer.getBytes("TEST"))
    val y:Rho = vrf.vrfProofToHash(pi_y)
    val h:Hash = ByteArrayWrapper(eta0)
    val genesisEntries: GenesisSet = List.range(0,numGenesisHolders).map(genEntry)
    val ledger:Mac = signMac(hashGen(genesisEntries,serializer), ByteArrayWrapper(FastCryptographicHash("coordId")),sk_sig,pk_sig)
    val cert:Cert = (pk_vrf,y,pi_y,pk_sig,new Ratio(BigInt(1),BigInt(1)),"genesis")
    val sig:KesSignature = sk_kes.sign(kes, h.data++serializer.getBytes(ledger)++serializer.getBytes(slot)++serializer.getBytes(cert)++rho++pi++serializer.getBytes(bn)++serializer.getBytes(ps))
    val genesisHeader:BlockHeader = (h,ledger,slot,cert,rho,pi,sig,pk_kes,bn,ps)
    println("Genesis Id:"+Base58.encode(hash(genesisHeader,serializer).data))
    Block(hash(genesisHeader,serializer),Some(genesisHeader),None,Some(genesisEntries))
  }

}
