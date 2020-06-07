package prosomo.primitives

import prosomo.components.Serializer
import prosomo.primitives.Types.{Hash, PublicKey, Sid, Signature}

/**
  * AMS 2020:
  * Simple message authentication code for verifying all messages from network
  */

case class Mac(dataHash:Hash, sid:Sid, signature:Signature, publicKey:PublicKey) {
  def verify(input:Hash,sig:Sig,serializer:Serializer):Boolean = {
    sig.verify(signature,dataHash.data++sid.data,publicKey) && dataHash == input
  }
}
