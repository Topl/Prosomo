package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeReturnBlocks
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes.{ReturnBlocksType,returnBlocksCode}

/**
  * AMS 2020:
  * Tine and Request block response,
  * blocks are passed one at a time with this message
  */

object ReturnBlocksSpec extends MessageSpecV1[ReturnBlocksType] with SerializationMethods {
  override val messageCode: MessageCode = returnBlocksCode
  override val messageName: String = "Return Blocks"

  override def parse(r: Reader): ReturnBlocksType = {
    returnFromBytes(r.getBytes(r.remaining))
  }

  override def serialize(obj: ReturnBlocksType, w: Writer): Unit = {
    w.putBytes(returnToBytes(obj))
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
