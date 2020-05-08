package prosomo.history

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.Serializer
import prosomo.primitives.{ByteStream, LDBStore, SharedData, Types}

import scala.concurrent.duration.MINUTES

class StateStorage(dir:String,serializer:Serializer) extends Types {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.{cacheSize,epochLength}
  val dbCacheSize = 4
  type DB = LDBStore

  private val stateStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .expireAfterAccess(10,MINUTES).maximumSize(dbCacheSize)
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch:BigInt):DB = {
        LDBStore(s"$dir/history/state/epoch_$epoch")
      }
    })

  private val etaStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .expireAfterAccess(10,MINUTES).maximumSize(dbCacheSize)
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch:BigInt):DB = {
        LDBStore(s"$dir/history/eta/epoch_$epoch")
      }
    })

  def refresh():Unit = {
    etaStoreCache.invalidateAll()
    stateStoreCache.invalidateAll()
  }

  private val stateCache:LoadingCache[SlotId,(State,Eta)] = CacheBuilder.newBuilder()
    .expireAfterAccess(10,MINUTES).maximumSize(cacheSize)
    .build[SlotId,(State,Eta)](new CacheLoader[SlotId,(State,Eta)] {
      def load(id:SlotId):(State,Eta) = {
        SharedData.throwDiskWarning
        (
          stateStoreCache.get(id._1/epochLength).get(id._2) match {
            case Some(bytes:ByteArrayWrapper) => {
              val byteStream = new ByteStream(bytes.data,DeserializeState)
              serializer.fromBytes(byteStream) match {
                case s:State => s
                case _ => Map()
              }
            }
            case None => Map()
          },
          etaStoreCache.get(id._1/epochLength).get(id._2) match {
            case Some(bytes:ByteArrayWrapper) => bytes.data
            case None => Array()
          }
        )
      }
    })

  def known(id:SlotId):Boolean = {
    stateCache.getIfPresent(id) match {
      case s:(State,Eta) => true
      case _ => stateStoreCache.get(id._1/epochLength).known(id._2)
    }
  }

  def known_then_load(id:SlotId):Boolean = {
    stateCache.get(id) match {
      case s:(State,Eta) => true
      case _ => false
    }
  }

  def add(id:SlotId,ls:State,eta:Eta):Unit = {
    if (!known(id)) {
      stateStoreCache.get(id._1/epochLength).update(Seq(),Seq(id._2 -> ByteArrayWrapper(serializer.getBytes(ls))))
      etaStoreCache.get(id._1/epochLength).update(Seq(),Seq(id._2 -> ByteArrayWrapper(eta)))
    }
    stateCache.put(id,(ls,eta))
  }

  def get(id:SlotId):Any = stateCache.get(id)

}
