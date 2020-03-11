package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.Serializer.DeserializeSendBlock
import prosomo.components.{Block, SerializationMethods}
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes._

import scala.util.{Failure, Success, Try}

object SendBlockSpec extends MessageSpec[SendBlockType] with SerializationMethods {
  override val messageCode: MessageCode = sendBlockCode
  override val messageName: String = "Send Block"

  override def parseBytes(bytes: Array[Byte]): Try[SendBlockType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeSendBlock)
    Try(fromBytes(msgBytes)) match {
      case Success(msg:(Block,Mac)) => Some(msg)
      case Failure(exception:Throwable) => {
        exception.printStackTrace()
        None
      }
      case _ => None
    }
  }

  override def toBytes(obj: SendBlockType): Array[Byte] = {
    getSendBlockBytes(obj.get)
  }

}
