package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeRequestBlock
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes._

import scala.util.{Failure, Success, Try}

object RequestBlockSpec extends MessageSpec[RequestBlockType] with SerializationMethods {
  override val messageCode: MessageCode = requestBlockCode
  override val messageName: String = "Request Block"

  override def parseBytes(bytes: Array[Byte]): Try[RequestBlockType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeRequestBlock)
    Try(fromBytes(msgBytes)) match {
      case Success(msg:(SlotId,Mac,Int)) => Some(msg)
      case Failure(exception:Throwable) => {
        exception.printStackTrace()
        None
      }
      case _ => None
    }
  }

  override def toBytes(obj: RequestBlockType): Array[Byte] = {
    getRequestBlockBytes(obj.get)
  }

}
