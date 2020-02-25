package prosomo.history

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import prosomo.components.Serializer
import prosomo.primitives.{ByteStream, Types}
import scorex.crypto.encode.Base58

class History(dir:String) extends Types {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.storageFlag

  var idMap:Map[Hash,(State,Eta)] = Map()

  val stateStore:LSMStore = {
    val iFile = new File(s"$dir/history/state")
    iFile.mkdirs()
    val store = new LSMStore(iFile)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        store.close()
      }
    })
    store
  }

  val etaStore:LSMStore = {
    val iFile = new File(s"$dir/history/eta")
    iFile.mkdirs()
    val store = new LSMStore(iFile)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        store.close()
      }
    })
    store
  }

  def known(id:BlockId):Boolean = if (storageFlag) {
    stateStore.versionIDExists(id)
  } else {
    idMap.keySet.contains(id)
  }

  def known(id:SlotId):Boolean = if (storageFlag) {
    known(id._2)
  } else {
    idMap.keySet.contains(id._2)
  }

  def add(id:Hash,ls:State,eta:Eta,serializer: Serializer) = if (storageFlag) {
    if (!known(id)) {
      stateStore.update(id,Seq(),Seq(id -> ByteArrayWrapper(serializer.getBytes(ls))))
      etaStore.update(id,Seq(),Seq(id -> ByteArrayWrapper(eta)))
    }
  } else {
    if (!known(id)) {
      idMap += (id -> (ls, eta))
    }
  }

  def get(id:Hash,serializer: Serializer):Any = if (storageFlag) {
    (
      stateStore.get(id) match {
        case Some(bytes:ByteArrayWrapper) => serializer.fromBytes(new ByteStream(bytes.data,DeserializeState))
        case None => None
      },
      etaStore.get(id) match {
        case Some(bytes:ByteArrayWrapper) => bytes.data
        case None => None
      }
    )
  } else {
    if (known(id)) {
      idMap(id)
    } else {
      println("Warning: Unknown id in history "+Base58.encode(id.data))
      0
    }
  }

}
