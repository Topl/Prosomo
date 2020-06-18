package prosomo.primitives

import scorex.crypto.hash.{CryptographicHash32, Digest32}

/**
  * AMS 2020:
  * Fast Crypto Hash as a class for actor system
  * Primary hash algorithm system wide
  * G_RO functionality
  */

class Fch extends CryptographicHash32 {
  val b2b = new B2b256
  override def hash(input: Message): Digest32 = b2b.hash(input)
}