package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeSendBlock
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes._

import scala.util.Try

object SendBlockSpec extends MessageSpec[SendBlockType] with SerializationMethods {
  override val messageCode: MessageCode = sendBlockCode
  override val messageName: String = "Send Block"

  override def parseBytes(bytes: Array[Byte]): Try[SendBlockType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeSendBlock)
    fromBytes(msgBytes) match {
      case msg:SendBlockType => msg
    }
  }

  override def toBytes(msg: SendBlockType): Array[Byte] = {
    getSendBlockBytes(msg)
  }

}
