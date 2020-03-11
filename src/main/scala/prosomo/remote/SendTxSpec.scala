package prosomo.remote

import bifrost.network.message.Message.MessageCode
import bifrost.network.message.MessageSpec
import prosomo.components.Serializer.DeserializeTransaction
import prosomo.components.{SerializationMethods, Transaction}
import prosomo.primitives.ByteStream
import prosomo.remote.SpecTypes._

import scala.util.{Failure, Success, Try}

object SendTxSpec extends MessageSpec[SendTxType] with SerializationMethods {
  override val messageCode: MessageCode = sendTxCode
  override val messageName: String = "Send Tx"

  override def parseBytes(bytes: Array[Byte]): Try[SendTxType] = Try{
    val msgBytes = new ByteStream(bytes,DeserializeTransaction)
    Try(fromBytes(msgBytes)) match {
      case Success(msg:Transaction) => Some(msg)
      case Failure(exception:Throwable) => {
        exception.printStackTrace()
        None
      }
      case _ => None
    }
  }

  override def toBytes(obj: SendTxType): Array[Byte] = {
    getBytes(obj.get)
  }

}