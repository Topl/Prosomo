package prosomo.primitives

import prosomo.components.Serializer
import prosomo.primitives.Types.{Hash, PublicKey, Sid, Signature}

case class Box(dataHash:Hash, sid:Sid, signature:Signature, publicKey:PublicKey) {
  def verify(input:Hash,sig:Sig,serializer:Serializer):Boolean = {
    sig.verify(signature,dataHash.data++sid.data,publicKey) && dataHash == input
  }
}
