package prosomo.primitives

class MalkinKey {
  var L:Tree[Array[Byte]] = Leaf(Array())
  var Si:Tree[Array[Byte]] = Leaf(Array())
  var sig:Array[Byte] = Array()
  var pki:Array[Byte] = Array()
  var rp:Array[Byte] = Array()

  def update(kes:Kes,t:Int) = {
    val updatedKey = kes.updateKey((L,Si,sig,pki,rp),t)
    L = updatedKey._1
    Si = updatedKey._2
    sig = updatedKey._3
    pki = updatedKey._4
    rp = updatedKey._5
  }

  def sign(kes:Kes,m:Array[Byte]): (Array[Byte],Array[Byte],Array[Byte]) = {
    kes.sign((L,Si,sig,pki,rp),m)
  }

  def getPublic(kes:Kes):Array[Byte] = {
    kes.publicKey((L,Si,sig,pki,rp))
  }

  def time(kes:Kes):Int = {
    kes.getKeyTimeStep((L,Si,sig,pki,rp))
  }
}

object MalkinKey {
  def apply(kes:Kes,seed:Array[Byte],t:Int):MalkinKey = {
    val keyData = kes.generateKey(seed)
    val updatedKeyData = kes.updateKey(keyData,t)
    val newKey = new MalkinKey
    newKey.L = updatedKeyData._1
    newKey.Si = updatedKeyData._2
    newKey.sig = updatedKeyData._3
    newKey.pki = updatedKeyData._4
    newKey.rp = updatedKeyData._5
    newKey
  }
  def apply(L:Tree[Array[Byte]],Si:Tree[Array[Byte]],sig:Array[Byte],pki:Array[Byte],rp:Array[Byte]):MalkinKey = {
    val newKey = new MalkinKey
    newKey.L = L
    newKey.Si = Si
    newKey.sig = sig
    newKey.pki = pki
    newKey.rp = rp
    newKey
  }
}
