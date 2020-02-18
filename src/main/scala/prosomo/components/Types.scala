package prosomo.components

import akka.actor.ActorRef
import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper
import com.google.common.primitives.{Bytes, Ints, Longs}

trait Types extends SimpleTypes {
  /**
    * main hash routine used in prosomo
    * @param input any bytes
    * @return wrapped byte array
    */

  def hash(input:ActorRef,serializer: Serializer): Hash = {
    hash(input.path.toString,serializer)
  }

  def hash(input:Slot,serializer: Serializer): Hash = {
    hash(input.toString+"SLOT_HASH",serializer)
  }

  def hash(input:(ActorRef,PublicKeys),serializer: Serializer): Hash = {
    ByteArrayWrapper(
      FastCryptographicHash(
        Bytes.concat(
          serializer.getBytes(input._1.path.toString),
          input._2._1,
          input._2._2,
          input._2._3
        )
      )
    )
  }

  def hashGenEntry(input:(Array[Byte], ByteArrayWrapper, BigInt),serializer: Serializer): Hash = {
    ByteArrayWrapper(
      FastCryptographicHash(
        Bytes.concat(
          input._1,
          input._2.data,
          input._3.toByteArray
        )
      )
    )
  }

  def hash(input:BlockHeader,serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(serializer.getBytes(input)))
  }

  def hash(input:Transaction,serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(serializer.getBytes(input)))
  }

  def hash(input:(List[Hash],Int,Int),serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(
      Bytes.concat(
        Bytes.concat(input._1.map(serializer.getBytes):_*),
          serializer.getBytes(input._2),
          serializer.getBytes(input._3)
        )
      )
    )
  }

  def hashGen(input:GenesisSet,serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(
      Bytes.concat(input.map(serializer.getBytes):_*)
    ))
  }

  def hash(input:TransactionSet,serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(
      Bytes.concat(input.map(serializer.getBytes):_*)
    ))
  }




  def hash(input:String,serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(serializer.getBytes(input)))
  }

}

object Types extends SimpleTypes