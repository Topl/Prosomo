package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.{DeserializeMac, DeserializeReturnBlocks}
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes.{ReturnBlocksType, returnBlocksCode}

/**
  * AMS 2020:
  * Tine and Request block response,
  * blocks are passed one at a time with this message
  */

object ReturnBlocksSpec extends MessageSpecV1[(Mac,ReturnBlocksType)] with SerializationMethods {
  override val messageCode: MessageCode = returnBlocksCode
  override val messageName: String = "Return Blocks"

  override def parse(r: Reader): (Mac,ReturnBlocksType) = {
    val mac = {
      fromBytes(new ByteStream(r.getBytes(mac_length),DeserializeMac)) match {
        case result:Mac@unchecked => result
      }
    }
    (mac,returnFromBytes(r.getBytes(r.remaining)))
  }

  override def serialize(obj: (Mac,ReturnBlocksType), w: Writer): Unit = {
    w.putBytes(getBytes(obj._1))
    w.putBytes(returnToBytes(obj._2))
  }

  def returnFromBytes(bytes: Array[Byte]): ReturnBlocksType = {
    val msgBytes = new ByteStream(bytes,DeserializeReturnBlocks)
    fromBytes(msgBytes) match {
      case msg:ReturnBlocksType@unchecked => msg
    }
  }

  def returnToBytes(msg: ReturnBlocksType): Array[Byte] = {
    getReturnBlocksBytes(msg)
  }

}
