package prosomo.primitives

import scorex.crypto.hash.{CryptographicHash32, Digest32}

//Fast Crypto Hash as a class for actor system
class Fch extends CryptographicHash32 {
  val b2b = new B2b256
  override def hash(input: Message): Digest32 = b2b.hash(input)
}