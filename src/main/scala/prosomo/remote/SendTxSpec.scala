package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.DeserializeSendTx
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes.{SendTxType, sendTxCode}

/**
  * AMS 2020:
  * Transactions are issued and broadcast to gossipers using this message,
  * txs are statefully checked and passed on if valid
  * new txs are added to mempool
  */

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
      case msg:SendTxType@unchecked => msg
    }
  }

  def sendToBytes(msg: SendTxType): Array[Byte] = {
    getSendTxBytes(msg)
  }
}