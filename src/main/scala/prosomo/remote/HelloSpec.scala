package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeHello
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes._

import scala.util.Try

object HelloSpec extends MessageSpec[HelloDataType] with SerializationMethods {
  override val messageCode: MessageCode = helloCode
  override val messageName: String = "Hello"

  override def parseBytes(bytes: Array[Byte]): Try[HelloDataType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeHello)
    fromBytes(msgBytes) match {
      case msg:HelloDataType => msg
    }
  }

  override def toBytes(msg: HelloDataType): Array[Byte] = {
    getHelloBytes(msg)
  }

}
