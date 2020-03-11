package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeHello
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes._

import scala.util.{Failure, Success, Try}

object HelloSpec extends MessageSpec[HelloDataType] with SerializationMethods {
  override val messageCode: MessageCode = helloCode
  override val messageName: String = "Hello"

  override def parseBytes(bytes: Array[Byte]): Try[HelloDataType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeHello)
    Try(fromBytes(msgBytes)) match {
      case Success(msg:(String,Mac)) => Some(msg)
      case Failure(exception:Throwable) => {
        exception.printStackTrace()
        None
      }
      case _ => None
    }
  }

  override def toBytes(obj: HelloDataType): Array[Byte] = {
    getHelloBytes(obj.get)
  }

}
