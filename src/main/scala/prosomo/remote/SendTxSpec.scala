package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeSendTx
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes._

import scala.util.Try

object SendTxSpec extends MessageSpec[SendTxType] with SerializationMethods {
  override val messageCode: MessageCode = sendTxCode
  override val messageName: String = "Send Tx"

  override def parseBytes(bytes: Array[Byte]): Try[SendTxType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeSendTx)
    fromBytes(msgBytes) match {
      case msg:SendTxType => msg
    }
  }

  override def toBytes(msg: SendTxType): Array[Byte] = {
    getSendTxBytes(msg)
  }

}