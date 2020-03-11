package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeRequestBlocks
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes._

import scala.util.{Failure, Success, Try}

object RequestBlocksSpec extends MessageSpec[RequestBlocksType] with SerializationMethods {
  override val messageCode: MessageCode = requestBlocksCode
  override val messageName: String = "Request Blocks"

  override def parseBytes(bytes: Array[Byte]): Try[RequestBlocksType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeRequestBlocks)
    Try(fromBytes(msgBytes)) match {
      case Success(msg:(SlotId,Int,Mac,Int)) => Some(msg)
      case Failure(exception:Throwable) => {
        exception.printStackTrace()
        None
      }
      case _ => None
    }
  }

  override def toBytes(obj: RequestBlocksType): Array[Byte] = {
    getRequestBlocksBytes(obj.get)
  }

}

