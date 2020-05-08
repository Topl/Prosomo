package prosomo.history

import java.io.File

import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Tine, Serializer}
import prosomo.primitives.{ByteStream, LDBStore, SimpleTypes}

class ChainStorage(dir:String) extends SimpleTypes {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.storageFlag

  var chainStore:LDBStore = LDBStore(s"$dir/history/chain")

  def refresh():Unit = {
    chainStore.refresh()
  }

  def restore(cid:Hash,serializer: Serializer):Tine = if (storageFlag) {
    chainStore.get(cid) match {
      case Some(bytes: ByteArrayWrapper) => {
        val byteStream: ByteStream = new ByteStream(bytes.data,DeserializeChain)
        serializer.fromBytes(byteStream) match {
          case c:Tine => c
          case _ => {
            new Tine
          }
        }
      }
      case None => {
        new Tine
      }
    }
  } else {
    new Tine
  }

  def store(chain:Tine, cid:Hash, serializer: Serializer):Unit  = {
    val cBytes = serializer.getBytes(chain)
    chainStore.update(Seq(),Seq(cid -> ByteArrayWrapper(cBytes)))
  }

}
