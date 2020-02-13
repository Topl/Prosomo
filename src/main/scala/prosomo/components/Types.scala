package prosomo.components

import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.ByteArrayWrapper

trait Types extends SimpleTypes {
  /**
    * main hash routine used in prosomo
    * @param input any bytes
    * @return wrapped byte array
    */
  def hash(input:Any,serializer: Serializer): Hash = {
    ByteArrayWrapper(FastCryptographicHash(serializer.getAnyBytes(input)))
  }

}

object Types extends SimpleTypes