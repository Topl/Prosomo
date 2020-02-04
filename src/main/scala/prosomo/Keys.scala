package prosomo

import io.iohk.iodb.ByteArrayWrapper

class Keys extends Types {
  var sk_vrf:PrivateKey = Array()
  var pk_vrf:PublicKey = Array()
  var sk_sig:PrivateKey = Array()
  var pk_sig:PublicKey = Array()
  var sk_kes:MalkinKey = _
  var pk_kes:PublicKey = Array()
  var publicKeys:PublicKeys = (Array(),Array(),Array())
  var pkw:PublicKeyW = ByteArrayWrapper(Array())
  var alpha:Double = 0.0
  var threshold:Double = 0.0

  def seedKeys(seed:Array[Byte],sig:Sig,vrf:Vrf,kes:Kes,t:Int) = {
    sk_vrf = vrf.vrfKeypair(seed)._1
    pk_vrf = vrf.vrfKeypair(seed)._2
    sk_sig = sig.createKeyPair(seed)._1
    pk_sig = sig.createKeyPair(seed)._2
    sk_kes = MalkinKey(kes,seed,t)
    pk_kes = sk_kes.getPublic(kes)
    publicKeys = (pk_sig,pk_vrf,pk_kes)
    pkw = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
  }
}

object Keys {
  def apply(seed:Array[Byte],sig:Sig,vrf:Vrf,kes:Kes,t:Int):Keys = {
    val newKeys = new Keys
    newKeys.sk_vrf = vrf.vrfKeypair(seed)._1
    newKeys.pk_vrf = vrf.vrfKeypair(seed)._2
    newKeys.sk_sig = sig.createKeyPair(seed)._1
    newKeys.pk_sig = sig.createKeyPair(seed)._2
    newKeys.sk_kes = MalkinKey(kes,seed,t)
    newKeys.pk_kes = newKeys.sk_kes.getPublic(kes)
    newKeys.publicKeys = (newKeys.pk_sig,newKeys.pk_vrf,newKeys.pk_kes)
    newKeys.pkw = ByteArrayWrapper(newKeys.pk_sig++newKeys.pk_vrf++newKeys.pk_kes)
    newKeys
  }
}
