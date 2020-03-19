package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeRequestBlock
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes._

import scala.util.{Success, Try}

object RequestBlockSpec extends MessageSpec[RequestBlockType] with SerializationMethods {
  override val messageCode: MessageCode = requestBlockCode
  override val messageName: String = "Request Block"

  override def parseBytes(bytes: Array[Byte]): Try[RequestBlockType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeRequestBlock)
    fromBytes(msgBytes) match {
      case Success(msg:RequestBlockType) => msg
    }
  }

  override def toBytes(msg: RequestBlockType): Array[Byte] = {
    getRequestBlockBytes(msg)
  }

}
