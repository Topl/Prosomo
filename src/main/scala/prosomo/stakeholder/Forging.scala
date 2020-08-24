package prosomo.stakeholder

import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.SendBlock
import prosomo.components.{Block, Tine}
import prosomo.primitives.{Keys, ForgingKey, Ratio, SharedData, Types}
import scorex.util.encode.Base58
import scala.math.BigInt
import scala.util.Try

/**
  * AMS 2020:
  * Forging routines for the genesis block and all other blocks,
  * Implements the Staking Procedure described in Ouroboros Genesis,
  * Newly forged blocks are immediately broadcast to network and placed first in line in tinePoolWithPrefix,
  * Forging is not allowed on tines with gaps longer than the slot window since these tines would never be adopted,
  * Forging is prohibited during bootstrapping to prevent futile blocks being broadcast during long tine recoveries
  */

trait Forging extends Members with Types {

  /**
    * Determines eligibility for a stakeholder to be a slot leader then calculates a block with epoch variables
    * */

  def forgeBlock(forgerKeys:Keys):Unit = if (!SharedData.limiterFlag) Try{
    val slot = globalSlot
    val pi_y: Pi = vrf.vrfProof(
      forgerKeys.sk_vrf, eta ++ serializer.getBytes(slot) ++ serializer.getBytes("TEST")
    )
    val y: Rho = vrf.vrfProofToHash(pi_y)
    val pb:BlockHeader = getBlockHeader(localChain.head).get
    assert(pb._3 != slot)
    val ps:Slot = pb._3

    def testThenForge(test:Rho,thr:Ratio): Unit = if (compare(test, thr)) {
      def metaInfo:String = {
        "forger_index:"+holderIndex.toString+",adversarial:"+adversary
          .toString+",eta:"+Base58.encode(eta)+",epoch:"+currentEpoch.toString
      }
      val bn:Int = pb._9 + 1
      val txs:TransactionSet = chooseLedger(forgerKeys.pkw,memPool,localState)
      val pi: Pi = vrf.vrfProof(
        forgerKeys.sk_vrf, eta ++ serializer.getBytes(slot) ++ serializer.getBytes("NONCE")
      )
      val rho: Rho = vrf.vrfProofToHash(pi)
      val h: Hash = hash(pb,serializer)
      val ledger:Hash = hash(txs,serializer)
      val cert: Cert = (forgerKeys.pk_vrf, y, pi_y, forgerKeys.pk_sig, thr,metaInfo)
      val kes_sig: ForgingSignature = forgerKeys.sk_kes.sign(
        kes,
        h.data
          ++serializer.getBytes(ledger)
          ++serializer.getBytes(slot)
          ++serializer.getBytes(cert)
          ++rho
          ++pi
          ++serializer.getBytes(bn)
          ++serializer.getBytes(ps)
      )
      val b:BlockHeader = (h, ledger, slot, cert, rho, pi, kes_sig, forgerKeys.pk_kes,bn,ps)
      val hb = hash(b,serializer)
      if (printFlag)
        println(s"Holder $holderIndex forged block $bn id:${Base58.encode(hb.data)} with ${txs.length} txs")
      val block = Block(hb,Some(b),Some(txs),None)
      blocks.add(block)
      updateLocalState(localState, (slot,block.id)) match {
        case Some(forgedState:State) =>
          send(selfWrapper,rng.shuffle(holders.filter(_ != selfWrapper))
            .take(numGossipersForge),SendBlock(block,selfWrapper))
          history.add((slot,block.id),forgedState,eta)
          blocksForged += 1
          val jobNumber = tineCounter
          tineCounter += 1
          tinePoolWithPrefix = tinePoolWithPrefix ++ Array((Tine((slot,block.id),rho),ps,jobNumber))
        case _ =>
          SharedData.throwError(holderIndex)
          println("error: invalid ledger in forged block")
      }
    }

    if (!bootStrapLock && slot - ps < slotWindow) {
      val test = stakingTestStrategy(y,ps,pb._9+1)
      if (f_dynamic) {
        testThenForge(test,threshold_cached(forgerKeys.alpha,slot-ps))
      } else {
        testThenForge(test,phi(forgerKeys.alpha))
      }
    }
  }

  def forgeGenBlock(eta0:Eta,
                    holderKeys:Map[Int,PublicKeyW],
                    pk_sig:PublicKey,
                    pk_vrf:PublicKey,
                    pk_kes:PublicKey,
                    sk_vrf:PublicKey,
                    sk_kes:ForgingKey
                   ): Block = {
    def genEntry(index:Int): (Array[Byte], PublicKeyW, BigInt) = {
      val initStake = {
        val out = stakeDistribution match {
          case "random" => initStakeMax*rng.nextDouble
          case "exp" => initStakeMax*math.exp(-stakeScale*index.toDouble)
          case "flat" => initStakeMax
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
      (
        genesisBytes.data,
        pkw,
        BigDecimal(initStake)
          .setScale(0, BigDecimal.RoundingMode.HALF_UP)
          .toBigInt
      )
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
    val ledger:Hash = hashGen(genesisEntries,serializer)
    val cert:Cert = (pk_vrf,y,pi_y,pk_sig,new Ratio(BigInt(1),BigInt(1)),"genesis")
    val sig:ForgingSignature = sk_kes.sign(
      kes,
      h.data
        ++serializer.getBytes(ledger)
        ++serializer.getBytes(slot)
        ++serializer.getBytes(cert)
        ++rho++pi++serializer.getBytes(bn)
        ++serializer.getBytes(ps)
    )
    val genesisHeader:BlockHeader = (h,ledger,slot,cert,rho,pi,sig,pk_kes,bn,ps)
    println("Genesis Id:"+Base58.encode(hash(genesisHeader,serializer).data))
    Block(hash(genesisHeader,serializer),Some(genesisHeader),None,Some(genesisEntries))
  }

}
