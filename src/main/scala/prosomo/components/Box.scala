package prosomo.components
import prosomo.components.Types._
import prosomo.primitives.Sig

case class Box(data:Any, sid:Sid, signature:Signature, publicKey:PublicKey) {
  def verify(sig:Sig,serializer:Serializer):Boolean = {
    sig.verify(signature,serializer.getAnyBytes(data)++sid.data,publicKey)
  }
}