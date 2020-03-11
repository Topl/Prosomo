package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeDiffuse
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes._

import scala.util.{Failure, Success, Try}

object DiffuseDataSpec extends MessageSpec[DiffuseDataType] with SerializationMethods {
  override val messageCode: MessageCode = diffuseCode
  override val messageName: String = "Diffuse"

  override def parseBytes(bytes: Array[Byte]): Try[DiffuseDataType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeDiffuse)
    Try(fromBytes(msgBytes)) match {
      case Success(msg:(String,PublicKeys,Mac)) => Some(msg)
      case Failure(exception:Throwable) => {
        exception.printStackTrace()
        None
      }
      case _ => None
    }
  }

  override def toBytes(obj:DiffuseDataType): Array[Byte] = {
    getDiffuseBytes(obj.get)
  }

}
