package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.{DeserializeMac, DeserializeSendBlock}
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes.{SendBlockType, sendBlockCode}

/**
  * AMS 2020:
  * Newly forged blocks are broadcast to gossipers with this message,
  * Tine building is triggered as a response and newly discovered blocks are passed on to gossipers,
  * New tines are added to tine pool and unknown parent ids are requested
  */

object SendBlockSpec extends MessageSpecV1[(Mac,SendBlockType)] with SerializationMethods {
  override val messageCode: MessageCode = sendBlockCode
  override val messageName: String = "Send Block"

  override def parse(r: Reader): (Mac,SendBlockType) = {
    val mac = {
      fromBytes(new ByteStream(r.getBytes(mac_length),DeserializeMac)) match {
        case result:Mac@unchecked => result
      }
    }
    (mac,sendFromBytes(r.getBytes(r.remaining)))
  }

  override def serialize(obj: (Mac,SendBlockType), w: Writer): Unit = {
    w.putBytes(getBytes(obj._1))
    w.putBytes(sendToBytes(obj._2))
  }

  def sendFromBytes(bytes: Array[Byte]): SendBlockType = {
    val msgBytes = new ByteStream(bytes,DeserializeSendBlock)
    fromBytes(msgBytes) match {
      case msg:SendBlockType@unchecked => msg
    }
  }

  def sendToBytes(msg: SendBlockType): Array[Byte] = {
    getSendBlockBytes(msg)
  }
}
