package prosomo.remote

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpecV1
import scorex.util.serialization.{Reader, Writer}
import prosomo.components.SerializationMethods
import prosomo.components.Serializer.{DeserializeMac, DeserializeSendTx}
import prosomo.primitives.{ByteStream, Mac}
import prosomo.remote.SpecTypes.{SendTxType, sendTxCode}

/**
  * AMS 2020:
  * Transactions are issued and broadcast to gossipers using this message,
  * txs are statefully checked and passed on if valid
  * new txs are added to mempool
  */

object SendTxSpec extends MessageSpecV1[(Mac,SendTxType)] with SerializationMethods {
  override val messageCode: MessageCode = sendTxCode
  override val messageName: String = "Send Tx"

  override def parse(r: Reader): (Mac,SendTxType) = {
    val mac = {
      fromBytes(new ByteStream(r.getBytes(mac_length),DeserializeMac)) match {
        case result:Mac@unchecked => result
      }
    }
    (mac,sendFromBytes(r.getBytes(r.remaining)))
  }

  override def serialize(obj: (Mac,SendTxType), w: Writer): Unit = {
    w.putBytes(getBytes(obj._1))
    w.putBytes(sendToBytes(obj._2))
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