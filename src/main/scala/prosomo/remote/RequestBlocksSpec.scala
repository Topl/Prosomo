package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeRequestBlocks
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes._

import scala.util.Try

object RequestBlocksSpec extends MessageSpec[RequestBlocksType] with SerializationMethods {
  override val messageCode: MessageCode = requestBlocksCode
  override val messageName: String = "Request Blocks"

  override def parseBytes(bytes: Array[Byte]): Try[RequestBlocksType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeRequestBlocks)
    fromBytes(msgBytes) match {
      case msg:RequestBlocksType => msg
    }
  }

  override def toBytes(msg: RequestBlocksType): Array[Byte] = {
    getRequestBlocksBytes(msg)
  }

}

