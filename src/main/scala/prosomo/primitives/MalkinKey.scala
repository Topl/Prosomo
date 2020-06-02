package prosomo.primitives
import com.google.common.primitives.Ints

class MalkinKey {
  var L:Tree[Array[Byte]] = Leaf(Array())
  var Si:Tree[Array[Byte]] = Leaf(Array())
  var sig:Array[Byte] = Array()
  var pki:Array[Byte] = Array()
  var rp:Array[Byte] = Array()
  var offset:Int = 0
  val fch = new Fch

  def update(kes:Kes,t:Int) = {
    val updatedKey = kes.updateKey((L,Si,sig,pki,rp),t-offset)
    L = updatedKey._1
    Si = updatedKey._2
    sig = updatedKey._3
    pki = updatedKey._4
    rp = updatedKey._5
  }

  def update_fast(kes:Kes,t:Int) = {
    val updatedKey = kes.updateKeyFast((L,Si,sig,pki,rp),t-offset)
    L = updatedKey._1
    Si = updatedKey._2
    sig = updatedKey._3
    pki = updatedKey._4
    rp = updatedKey._5
  }

  def sign(kes:Kes,m:Array[Byte]): (Array[Byte],Array[Byte],Array[Byte],Int,Array[Byte]) = {
    val out = kes.sign((L,Si,sig,pki,rp),m)
    (out._1,out._2,out._3,offset,kes.publicKey((L,Si,sig,pki,rp)))
  }

  def getPublic(kes:Kes):Array[Byte] = {
    val pk_kes = kes.publicKey((L,Si,sig,pki,rp))
    fch.hash(Ints.toByteArray(offset)++pk_kes)
  }

  def time(kes:Kes):Int = {
    kes.getKeyTimeStep((L,Si,sig,pki,rp)) + offset
  }
}

object MalkinKey {
  def apply(kes:Kes,seed:Array[Byte],t:Int):MalkinKey = {
    val keyData = kes.generateKey(seed)
    val newKey = new MalkinKey
    newKey.L = keyData._1
    newKey.Si = keyData._2
    newKey.sig = keyData._3
    newKey.pki = keyData._4
    newKey.rp = keyData._5
    newKey.offset = t
    newKey
  }
  def apply(L:Tree[Array[Byte]],Si:Tree[Array[Byte]],sig:Array[Byte],pki:Array[Byte],rp:Array[Byte],offset:Int):MalkinKey = {
    val newKey = new MalkinKey
    newKey.L = L
    newKey.Si = Si
    newKey.sig = sig
    newKey.pki = pki
    newKey.rp = rp
    newKey.offset = offset
    newKey
  }
}
