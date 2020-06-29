package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.{DeserializeMac, DeserializeRequestTine}
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes.{RequestTineType, requestTineCode}

/**
  * AMS 2020:
  * Used for bootstrapping when the tine building procedure reaches a set depth
  * Providerized response, one provider per stakeholder instance sends a limited rate of blocks per second
  */

object RequestTineSpec extends MessageSpecV1[(Mac,RequestTineType)] with SerializationMethods {
  override val messageCode: MessageCode = requestTineCode
  override val messageName: String = "Request Blocks"

  override def parse(r: Reader): (Mac,RequestTineType) = {
    val mac = {
      fromBytes(new ByteStream(r.getBytes(mac_length),DeserializeMac)) match {
        case result:Mac@unchecked => result
      }
    }
    (mac,requestFromBytes(r.getBytes(r.remaining)))
  }

  override def serialize(obj: (Mac,RequestTineType), w: Writer): Unit = {
    w.putBytes(getBytes(obj._1))
    w.putBytes(requestToBytes(obj._2))
  }

  def requestFromBytes(bytes: Array[Byte]): RequestTineType = {
    val msgBytes = new ByteStream(bytes,DeserializeRequestTine)
    fromBytes(msgBytes) match {
      case msg:RequestTineType@unchecked => msg
    }
  }

  def requestToBytes(msg: RequestTineType): Array[Byte] = {
    getRequestTineBytes(msg)
  }
}

