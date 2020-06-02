package prosomo.primitives

import scorex.crypto.hash._

/*
  Blake2b-256 bit message digest
 */

class B2b256 extends Blake2b[Digest32] with CryptographicHash32 {
  override def hash(input: Message): Digest32 = Digest32 @@ internalHash(input)

  override def prefixedHash(prefix: Byte, inputs: Array[Byte]*): Digest32 =
    Digest32 @@ internalPrefixedHash(prefix, inputs: _*)
}
