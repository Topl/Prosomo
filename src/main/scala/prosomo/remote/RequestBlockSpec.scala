package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeRequestBlock
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes.{RequestBlockType, requestBlockCode}

/**
  * AMS 2020:
  * Holders build tines by requesting individual block ids
  */

object RequestBlockSpec extends MessageSpecV1[RequestBlockType] with SerializationMethods {
  override val messageCode: MessageCode = requestBlockCode
  override val messageName: String = "Request Block"

  override def parse(r:Reader): RequestBlockType = {
    requestFromBytes(r.getBytes(r.remaining))
  }

  override def serialize(obj: RequestBlockType, w: Writer): Unit = {
    w.putBytes(requestToBytes(obj))
  }

  def requestFromBytes(bytes: Array[Byte]): RequestBlockType = {
    val msgBytes = new ByteStream(bytes,DeserializeRequestBlock)
    fromBytes(msgBytes) match {
      case msg:RequestBlockType@unchecked => msg
    }
  }

  def requestToBytes(msg: RequestBlockType): Array[Byte] = {
    getRequestBlockBytes(msg)
  }

}
