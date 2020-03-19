package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeReturnBlocks
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes._

import scala.util.Try

object ReturnBlocksSpec extends MessageSpec[ReturnBlocksType] with SerializationMethods {
  override val messageCode: MessageCode = returnBlocksCode
  override val messageName: String = "Return Blocks"

  override def parseBytes(bytes: Array[Byte]): Try[ReturnBlocksType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeReturnBlocks)
    fromBytes(msgBytes) match {
      case msg:ReturnBlocksType => msg
    }
  }

  override def toBytes(msg: ReturnBlocksType): Array[Byte] = {
    getReturnBlocksBytes(msg)
  }

}
