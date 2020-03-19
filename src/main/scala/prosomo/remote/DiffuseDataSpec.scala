package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeDiffuse
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes._

import scala.util.Try

object DiffuseDataSpec extends MessageSpec[DiffuseDataType] with SerializationMethods {
  override val messageCode: MessageCode = diffuseCode
  override val messageName: String = "Diffuse"

  override def parseBytes(bytes: Array[Byte]): Try[DiffuseDataType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeDiffuse)
    fromBytes(msgBytes) match {
      case msg:DiffuseDataType => msg
    }
  }

  override def toBytes(msg:DiffuseDataType): Array[Byte] = {
    getDiffuseBytes(msg)
  }

}
