package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeDiffuse
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes.{DiffuseDataType,diffuseCode}


object DiffuseDataSpec extends MessageSpecV1[DiffuseDataType] with SerializationMethods {
  override val messageCode: MessageCode = diffuseCode
  override val messageName: String = "Diffuse"

  override def parse(r: Reader):DiffuseDataType = {
    diffuseFromBytes(r.getBytes(r.remaining))
  }

  override def serialize(obj: DiffuseDataType, w: Writer): Unit = {
    w.putBytes(diffuseToBytes(obj))
  }

  def diffuseFromBytes(bytes: Array[Byte]): DiffuseDataType = {
    val msgBytes = new ByteStream(bytes,DeserializeDiffuse)
    fromBytes(msgBytes) match {
      case msg:DiffuseDataType@unchecked => msg
    }
  }

  def diffuseToBytes(msg:DiffuseDataType): Array[Byte] = {
    getDiffuseBytes(msg)
  }
}
