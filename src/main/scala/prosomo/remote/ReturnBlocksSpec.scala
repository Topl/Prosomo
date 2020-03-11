package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.Serializer.DeserializeReturnBlocks
import prosomo.components.{Block, SerializationMethods}
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes._

import scala.util.{Failure, Success, Try}

object ReturnBlocksSpec extends MessageSpec[ReturnBlocksType] with SerializationMethods {
  override val messageCode: MessageCode = returnBlocksCode
  override val messageName: String = "Return Blocks"

  override def parseBytes(bytes: Array[Byte]): Try[ReturnBlocksType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeReturnBlocks)
    Try(fromBytes(msgBytes)) match {
      case Success(msg:(List[Block],Mac,Int)) => Some(msg)
      case Failure(exception:Throwable) => {
        exception.printStackTrace()
        None
      }
      case _ => None
    }
  }

  override def toBytes(obj: ReturnBlocksType): Array[Byte] = {
    getReturnBlocksBytes(obj.get)
  }

}
