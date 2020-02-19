package prosomo.components

import java.io.File
import io.iohk.iodb.LSMStore

class ChainStorage(dir:String) {
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



}
