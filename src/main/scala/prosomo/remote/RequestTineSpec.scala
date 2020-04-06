package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeRequestTine
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes._

import scala.util.Try

object RequestTineSpec extends MessageSpec[RequestTineType] with SerializationMethods {
  override val messageCode: MessageCode = requestTineCode
  override val messageName: String = "Request Blocks"

  override def parseBytes(bytes: Array[Byte]): Try[RequestTineType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeRequestTine)
    fromBytes(msgBytes) match {
      case msg:RequestTineType => msg
    }
  }

  override def toBytes(msg: RequestTineType): Array[Byte] = {
    getRequestTineBytes(msg)
  }

}

