package prosomo.components

import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper

trait Types extends SimpleTypes {
  /**
    * main hash routine used in prosomo
    * @param input any bytes
    * @return wrapped byte array
    */
  def hash(input:BlockHeader,serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(serializer.getBytes(input)))
  }

  def hash(input:String,serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(serializer.getBytes(input)))
  }

}

object Types extends SimpleTypes