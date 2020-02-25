package prosomo.primitives

import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper

import scala.util.Try

class Keys extends Types {
  var sk_vrf:PrivateKey = Array()
  var pk_vrf:PublicKey = Array()
  var sk_sig:PrivateKey = Array()
  var pk_sig:PublicKey = Array()
  var sk_kes:MalkinKey = _
  var pk_kes:PublicKey = Array()
  var publicKeys:PublicKeys = (Array(),Array(),Array())
  var pkw:PublicKeyW = ByteArrayWrapper(Array())
  var alpha:Ratio = new Ratio(BigInt(0),BigInt(1))
  var threshold:Ratio = new Ratio(BigInt(0),BigInt(1))

  //for testing only, not secure entropy sampling
  def seedKeys(seed:Array[Byte],sig:Sig,vrf:Vrf,kes:Kes,t:Int):Unit = {
    sk_vrf = vrf.vrfKeypair(seed)._1
    pk_vrf = vrf.vrfKeypair(seed)._2
    sk_sig = sig.createKeyPair(FastCryptographicHash(seed))._1
    pk_sig = sig.createKeyPair(FastCryptographicHash(seed))._2
    sk_kes = MalkinKey(kes,FastCryptographicHash(FastCryptographicHash(seed)),t)
    pk_kes = sk_kes.getPublic(kes)
    publicKeys = (pk_sig,pk_vrf,pk_kes)
    pkw = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
  }
}

object Keys {
  //for testing only, not secure entropy sampling
  def apply(seed:Array[Byte],sig:Sig,vrf:Vrf,kes:Kes,t:Int):Keys = {
    val newKeys = new Keys
    newKeys.sk_vrf = vrf.vrfKeypair(seed)._1
    newKeys.pk_vrf = vrf.vrfKeypair(seed)._2
    newKeys.sk_sig = sig.createKeyPair(FastCryptographicHash(seed))._1
    newKeys.pk_sig = sig.createKeyPair(FastCryptographicHash(seed))._2
    newKeys.sk_kes = MalkinKey(kes,FastCryptographicHash(FastCryptographicHash(seed)),t)
    newKeys.pk_kes = newKeys.sk_kes.getPublic(kes)
    newKeys.publicKeys = (newKeys.pk_sig,newKeys.pk_vrf,newKeys.pk_kes)
    newKeys.pkw = ByteArrayWrapper(newKeys.pk_sig++newKeys.pk_vrf++newKeys.pk_kes)
    newKeys
  }

  //secure seeding with valid entropy
  def seedKeysSecure(seed1:Array[Byte],seed2:Array[Byte],seed3:Array[Byte],sig:Sig,vrf:Vrf,kes:Kes,t:Int):Try[Keys] = Try {
    require(
      seed1.length>=32
        && seed2.length>=32
        && seed3.length>=32
        && ByteArrayWrapper(seed1) != ByteArrayWrapper(seed2)
        && ByteArrayWrapper(seed2) != ByteArrayWrapper(seed3)
        && ByteArrayWrapper(seed3) != ByteArrayWrapper(seed1)
      ,"Invalid seed entropy")
    val out = new Keys
    out.sk_sig = sig.createKeyPair(seed1)._1
    out.pk_sig = sig.createKeyPair(seed1)._2
    out.sk_vrf = vrf.vrfKeypair(seed2)._1
    out.pk_vrf = vrf.vrfKeypair(seed2)._2
    out.sk_kes = MalkinKey(kes,FastCryptographicHash(FastCryptographicHash(seed3)),t)
    out.pk_kes = out.sk_kes.getPublic(kes)
    out.publicKeys = (out.pk_sig,out.pk_vrf,out.pk_kes)
    out.pkw = ByteArrayWrapper(out.pk_sig++out.pk_vrf++out.pk_kes)
    out
  }
}
