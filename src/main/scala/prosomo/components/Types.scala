package prosomo.components

import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper
import com.google.common.primitives.{Bytes, Ints, Longs}

trait Types extends SimpleTypes {
  /**
    * main hash routine used in prosomo
    * @param input any bytes
    * @return wrapped byte array
    */
  def hash(input:BlockHeader,serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(serializer.getBytes(input)))
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