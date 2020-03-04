package prosomo.history

import java.io.File

import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Chain, Serializer}
import prosomo.primitives.{ByteStream, LDBStore, SimpleTypes}

class ChainStorage(dir:String) extends SimpleTypes {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.storageFlag

  val chainStore:LDBStore = {
    val iFile = new File(s"$dir/history/chain")
    iFile.mkdirs()
    val store = new LDBStore(iFile)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        store.close()
      }
    })
    store
  }

  def restore(cid:Hash,serializer: Serializer):Chain = if (storageFlag) {
    chainStore.get(cid) match {
      case Some(bytes: ByteArrayWrapper) => serializer.fromBytes(new ByteStream(bytes.data,DeserializeChain)) match {
        case c:Chain => c
        case _ => {
          new Chain
        }
      }
      case None => {
        new Chain
      }
    }
  } else {
    new Chain
  }

  def store(chain:Chain,cid:Hash,serializer: Serializer):Unit  = {
    val cBytes = serializer.getBytes(chain)
    chainStore.update(Seq(),Seq(cid -> ByteArrayWrapper(cBytes)))
  }

}
