package prosomo.components

import com.google.common.primitives.{Ints, Longs}

case class ByteStream(var data:Array[Byte],co:Any) {
  def get(n:Int):Array[Byte] = {
    val out = data.take(n)
    data = data.drop(n)
    out
  }

  def getInt: Int = {
    Ints.fromByteArray(get(4))
  }

  def getLong: Long = {
    Longs.fromByteArray(get(8))
  }

  def empty:Boolean = data.isEmpty
}
