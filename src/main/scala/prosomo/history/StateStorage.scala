package prosomo.history

import java.io.File

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.Serializer
import prosomo.primitives.{ByteStream, LDBStore, SharedData, Types}
import scorex.crypto.encode.Base58

import scala.concurrent.duration.MINUTES

class StateStorage(dir:String) extends Types {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.{storageFlag,cacheSize}

  val serializer:Serializer = new Serializer

  var idMap:Map[Hash,(State,Eta)] = Map()

  val stateStore:LDBStore = {
    val iFile = new File(s"$dir/history/state")
    iFile.mkdirs()
    val store = new LDBStore(iFile)
//    Runtime.getRuntime.addShutdownHook(new Thread() {
//      override def run(): Unit = {
//        store.close()
//      }
//    })
    store
  }

  val etaStore:LDBStore = {
    val iFile = new File(s"$dir/history/eta")
    iFile.mkdirs()
    val store = new LDBStore(iFile)
    val newThread = new Thread() {
      override def run(): Unit = {
        store.close()
      }
    }
    Runtime.getRuntime.addShutdownHook(newThread)
    store
  }

  private val stateLoader:CacheLoader[SlotId,(State,Eta)] = new CacheLoader[SlotId,(State,Eta)] {
    def load(id:SlotId):(State,Eta) = {
      SharedData.throwDiskWarning
      (
        stateStore.get(id._2) match {
          case Some(bytes:ByteArrayWrapper) => {
            val byteStream = new ByteStream(bytes.data,DeserializeState)
            serializer.fromBytes(byteStream) match {
              case s:State => s
              case _ => Map()
            }
          }
          case None => Map()
        },
        etaStore.get(id._2) match {
          case Some(bytes:ByteArrayWrapper) => bytes.data
          case None => Array()
        }
      )
    }
  }

  private val stateCache:LoadingCache[SlotId,(State,Eta)] = CacheBuilder.newBuilder()
    .expireAfterAccess(10,MINUTES).maximumSize(cacheSize)
    .build[SlotId,(State,Eta)](stateLoader)


  def known(id:SlotId):Boolean = if (storageFlag) {
    stateCache.getIfPresent(id) match {
      case s:(State,Eta) => true
      case _ => stateStore.known(id._2)
    }
  } else {
    idMap.keySet.contains(id._2)
  }

  def known_then_load(id:SlotId):Boolean = if (storageFlag) {
    stateCache.get(id) match {
      case s:(State,Eta) => true
      case _ => false
    }
  } else {
    idMap.keySet.contains(id._2)
  }

  def add(id:SlotId,ls:State,eta:Eta):Unit = if (storageFlag) {
    if (!known(id)) {
      stateStore.update(Seq(),Seq(id._2 -> ByteArrayWrapper(serializer.getBytes(ls))))
      etaStore.update(Seq(),Seq(id._2 -> ByteArrayWrapper(eta)))
    }
    stateCache.put(id,(ls,eta))
  } else {
    if (!known(id)) {
      idMap += (id._2 -> (ls, eta))
    }
  }

  def get(id:SlotId):Any = if (storageFlag) {
    stateCache.get(id)
  } else {
    if (known(id)) {
      idMap(id._2)
    } else {
      println("Warning: Unknown id in history "+Base58.encode(id._2.data))
      0
    }
  }

}
