package prosomo.components

import java.io.File

import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.LSMStore
import io.iohk.iodb.ByteArrayWrapper

class ChainStorage(dir:String) extends SimpleTypes {
  import prosomo.primitives.Parameters.storageFlag
  import prosomo.components.Serializer._

  val chainStore:LSMStore = {
    val iFile = new File(s"$dir/history/chain")
    iFile.mkdirs()
    val store = new LSMStore(iFile)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        store.close()
      }
    })
    store
  }

  def get(cid:Hash,serializer: Serializer) = {
    if (chainStore.versionIDExists(cid)) {
      chainStore.get(cid) match {
        case Some(bytes: ByteArrayWrapper) => serializer.fromBytes(new ByteStream(bytes.data,DeserializeChain)) match {
          case c:Chain=>c
          case _ => new Chain
        }
        case None => new Chain
      }

    } else {
      new Chain
    }
  }

  def store(chain:Chain,serializer: Serializer):Hash  = {
    val cBytes = serializer.getBytes(chain)
    val cid = ByteArrayWrapper(FastCryptographicHash(cBytes))
    if (!chainStore.versionIDExists(cid)) {
      chainStore.update(cid,Seq(),Seq(cid -> ByteArrayWrapper(cBytes)))
    }
    cid
  }

}
