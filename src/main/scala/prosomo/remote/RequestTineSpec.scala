package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeRequestTine
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes.{RequestTineType, requestTineCode}

/**
  * AMS 2020:
  * Used for bootstrapping when the tine building procedure reaches a set depth
  * Providerized response, one provider per stakeholder instance sends a limited rate of blocks per second
  */

object RequestTineSpec extends MessageSpecV1[RequestTineType] with SerializationMethods {
  override val messageCode: MessageCode = requestTineCode
  override val messageName: String = "Request Blocks"

  override def parse(r: Reader): RequestTineType = {
    requestFromBytes(r.getBytes(r.remaining))
  }

  override def serialize(obj: RequestTineType, w: Writer): Unit = {
    w.putBytes(requestToBytes(obj))
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

