package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeHoldersFromRemote
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes.holdersFromRemote

object HoldersFromRemoteSpec extends MessageSpecV1[List[String]] with SerializationMethods {
  override val messageCode: MessageCode = holdersFromRemote
  override val messageName: String = "Holders from remote"

  override def parse(r: Reader): List[String] = {
    holdersFromBytes(r.getBytes(r.remaining))
  }

  override def serialize(obj: List[String], w: Writer): Unit = {
    w.putBytes(holdersToBytes(obj))
  }

  def holdersFromBytes(bytes: Array[Byte]): List[String] = {
    val msgBytes = new ByteStream(bytes,DeserializeHoldersFromRemote)
    fromBytes(msgBytes) match {
      case msg:List[String] => msg
    }
  }

  def holdersToBytes(msg: List[String]): Array[Byte] = {
    getHoldersBytes(msg)
  }
}