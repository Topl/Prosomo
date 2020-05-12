package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeSendTx
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes.{SendTxType, sendTxCode}

object SendTxSpec extends MessageSpecV1[SendTxType] with SerializationMethods {
  override val messageCode: MessageCode = sendTxCode
  override val messageName: String = "Send Tx"

  override def parse(r: Reader): SendTxType = {
    sendFromBytes(r.getBytes(r.remaining))
  }

  override def serialize(obj: SendTxType, w: Writer): Unit = {
    w.putBytes(sendToBytes(obj))
  }

  def sendFromBytes(bytes: Array[Byte]): SendTxType = {
    val msgBytes = new ByteStream(bytes,DeserializeSendTx)
    fromBytes(msgBytes) match {
      case msg:SendTxType => msg
    }
  }

  def sendToBytes(msg: SendTxType): Array[Byte] = {
    getSendTxBytes(msg)
  }
}