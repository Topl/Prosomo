package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeMac
import prosomo.primitives.{ByteStream,Mac}
import prosomo.remote.SpecTypes.helloCode

/**
  * AMS 2020:
  * Hello message for fetch info functionality specified in Genesis
  */

object HelloSpec extends MessageSpecV1[(Mac,Array[Byte])] with SerializationMethods {
  override val messageCode: MessageCode = helloCode
  override val messageName: String = "Hello"

  override def parse(r: Reader):(Mac,Array[Byte]) = {
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
