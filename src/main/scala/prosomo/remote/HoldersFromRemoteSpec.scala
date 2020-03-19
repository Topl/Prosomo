package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeHoldersFromRemote
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes.holdersFromRemote

import scala.util.Try

object HoldersFromRemoteSpec extends MessageSpec[List[String]] with SerializationMethods {
  override val messageCode: MessageCode = holdersFromRemote
  override val messageName: String = "Holders from remote"

  override def parseBytes(bytes: Array[Byte]): Try[List[String]] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeHoldersFromRemote)
    fromBytes(msgBytes) match {
      case msg:List[String] => msg
    }
  }

  override def toBytes(msg: List[String]): Array[Byte] = {
    getHoldersBytes(msg)
  }

}