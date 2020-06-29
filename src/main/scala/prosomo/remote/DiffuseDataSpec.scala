package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.{DeserializeDiffuse, DeserializeMac}
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes.{DiffuseDataType, diffuseCode}

/**
  * AMS 2020:
  * Diffuse data is used to identify peers with their forging public address, optional to broadcast
  */

object DiffuseDataSpec extends MessageSpecV1[(Mac,DiffuseDataType)] with SerializationMethods {
  override val messageCode: MessageCode = diffuseCode
  override val messageName: String = "Diffuse"

  override def parse(r: Reader):(Mac,DiffuseDataType) = {
    val mac = {
      fromBytes(new ByteStream(r.getBytes(mac_length),DeserializeMac)) match {
        case result:Mac@unchecked => result
      }
    }
    (mac,diffuseFromBytes(r.getBytes(r.remaining)))
  }

  override def serialize(obj: (Mac,DiffuseDataType), w: Writer): Unit = {
    w.putBytes(getBytes(obj._1))
    w.putBytes(diffuseToBytes(obj._2))
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
