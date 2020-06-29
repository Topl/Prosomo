package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.{DeserializeMac, DeserializeRequestBlock}
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes.{RequestBlockType, requestBlockCode}

/**
  * AMS 2020:
  * Holders build tines by requesting individual block ids
  */

object RequestBlockSpec extends MessageSpecV1[(Mac,RequestBlockType)] with SerializationMethods {
  override val messageCode: MessageCode = requestBlockCode
  override val messageName: String = "Request Block"

  override def parse(r:Reader): (Mac,RequestBlockType) = {
    val mac = {
      fromBytes(new ByteStream(r.getBytes(mac_length),DeserializeMac)) match {
        case result:Mac@unchecked => result
      }
    }
    (mac,requestFromBytes(r.getBytes(r.remaining)))
  }

  override def serialize(obj: (Mac,RequestBlockType), w: Writer): Unit = {
    w.putBytes(getBytes(obj._1))
    w.putBytes(requestToBytes(obj._2))
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
