package prosomo.history

import java.io.File

import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import prosomo.components.{Chain, Serializer}
import prosomo.primitives.{ByteStream, SimpleTypes}

class ChainStorage(dir:String) extends SimpleTypes {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.storageFlag

  val checkPoint = ByteArrayWrapper(FastCryptographicHash("CHECKPOINT"))

  val chainStore:LSMStore = {
    val iFile = new File(s"$dir/history/chain")
    iFile.mkdirs()
    val store = new LSMStore(iFile,maxFileSize = 1024)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        store.close()
      }
    })
    store.lastVersionID match {
      case None => store.update(checkPoint,Seq(),Seq())
      case _ =>
    }
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
    chainStore.rollback(checkPoint)
    chainStore.update(cid,Seq(),Seq(cid -> ByteArrayWrapper(cBytes)))
  }

}
