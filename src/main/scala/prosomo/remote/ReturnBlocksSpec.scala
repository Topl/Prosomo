package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeMac
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes.returnBlocksCode

/**
  * AMS 2020:
  * Tine and Request block response,
  * blocks are passed one at a time with this message
  */

object ReturnBlocksSpec extends MessageSpecV1[(Mac,Array[Byte])] with SerializationMethods {
  override val messageCode: MessageCode = returnBlocksCode
  override val messageName: String = "Return Blocks"

  override def parse(r: Reader): (Mac,Array[Byte]) = {
    val mac = {
      fromBytes(new ByteStream(r.getBytes(mac_length),DeserializeMac)) match {
        case result:Mac@unchecked => result
      }
    }
    (mac,r.getBytes(r.remaining))
  }

  override def serialize(obj: (Mac,Array[Byte]), w: Writer): Unit = {
    w.putBytes(getBytes(obj._1))
    w.putBytes(obj._2)
  }
}
