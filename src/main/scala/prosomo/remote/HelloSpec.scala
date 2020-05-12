package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeHello
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes.{HelloDataType,helloCode}

object HelloSpec extends MessageSpecV1[HelloDataType] with SerializationMethods {
  override val messageCode: MessageCode = helloCode
  override val messageName: String = "Hello"

  override def parse(r: Reader):HelloDataType = {
    helloFromBytes(r.getBytes(r.remaining))
  }

  override def serialize(obj: HelloDataType, w: Writer): Unit = {
    w.putBytes(helloToBytes(obj))
  }

  def helloFromBytes(bytes: Array[Byte]): HelloDataType = {
    val msgBytes = new ByteStream(bytes,DeserializeHello)
    fromBytes(msgBytes) match {
      case msg:HelloDataType => msg
    }
  }

  def helloToBytes(msg: HelloDataType): Array[Byte] = {
    getHelloBytes(msg)
  }
}
