package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.{DeserializeHello,DeserializeMac}
import prosomo.primitives.{ByteStream,Mac}
import prosomo.remote.SpecTypes.{HelloDataType,helloCode}

/**
  * AMS 2020:
  * Hello message for fetch info functionality specified in Genesis
  */

object HelloSpec extends MessageSpecV1[(Mac,HelloDataType)] with SerializationMethods {
  override val messageCode: MessageCode = helloCode
  override val messageName: String = "Hello"

  override def parse(r: Reader):(Mac,HelloDataType) = {
    val mac = {
      fromBytes(new ByteStream(r.getBytes(mac_length),DeserializeMac)) match {
        case result:Mac@unchecked => result
      }
    }
    (mac,helloFromBytes(r.getBytes(r.remaining)))
  }

  override def serialize(obj: (Mac,HelloDataType), w: Writer): Unit = {
    w.putBytes(getBytes(obj._1))
    w.putBytes(helloToBytes(obj._2))
  }

  def helloFromBytes(bytes: Array[Byte]): HelloDataType = {
    val msgBytes = new ByteStream(bytes,DeserializeHello)
    fromBytes(msgBytes) match {
      case msg:HelloDataType@unchecked => msg
    }
  }

  def helloToBytes(msg: HelloDataType): Array[Byte] = {
    getHelloBytes(msg)
  }
}
