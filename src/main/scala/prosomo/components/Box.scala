package prosomo.components
import prosomo.components.Types._
import prosomo.primitives.Sig

case class Box(dataHash:Hash, sid:Sid, signature:Signature, publicKey:PublicKey) {
  def verify(input:Hash,sig:Sig,serializer:Serializer):Boolean = {
    sig.verify(signature,dataHash.data++sid.data,publicKey) && dataHash == input
  }
}