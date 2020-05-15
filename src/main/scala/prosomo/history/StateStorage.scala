package prosomo.history

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache, RemovalNotification}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.Serializer
import prosomo.primitives.{ByteStream, LDBStore, SharedData, Types}

import scala.util.Try

class StateStorage(dir:String,serializer:Serializer) extends Types {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.{cacheSize,epochLength}
  val dbCacheSize = 4
  type DB = LDBStore

  private val stateStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .maximumSize(dbCacheSize)
    .removalListener((notification: RemovalNotification[BigInt, DB]) => {
      notification.getValue.close()
    })
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch:BigInt):DB = {
        LDBStore(s"$dir/history/state/epoch_$epoch")
      }
    })

  private val etaStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .maximumSize(dbCacheSize)
    .removalListener((notification: RemovalNotification[BigInt, DB]) => {
      notification.getValue.close()
    })
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch:BigInt):DB = {
        LDBStore(s"$dir/history/eta/epoch_$epoch")
      }
    })

  def refresh:Unit = {
    etaStoreCache.asMap().keySet().forEach(etaStoreCache.get(_).refresh())
    stateStoreCache.asMap().keySet().forEach(stateStoreCache.get(_).refresh())
  }

  private val stateCache:LoadingCache[SlotId,(State,Eta)] = CacheBuilder.newBuilder()
    .maximumSize(cacheSize)
    .build[SlotId,(State,Eta)](new CacheLoader[SlotId,(State,Eta)] {
      def load(id:SlotId):(State,Eta) = {
        SharedData.throwDiskWarning
        (
          stateStoreCache.get(id._1/epochLength).get(id._2).get match {
            case bytes:ByteArrayWrapper => {
              val byteStream = new ByteStream(bytes.data,DeserializeState)
              serializer.fromBytes(byteStream) match {
                case s:State@unchecked => s
              }
            }
          },
          etaStoreCache.get(id._1/epochLength).get(id._2).get match {
            case bytes:ByteArrayWrapper => bytes.data
          }
        )
      }
    })

  def known(id:SlotId):Boolean = {
    Try{stateCache.getIfPresent(id)}.toOption match {
      case Some(s:(State,Eta)) => true
      case _ => stateStoreCache.get(id._1/epochLength).known(id._2)
    }
  }

  def known_then_load(id:SlotId):Boolean = {
    Try{stateCache.get(id)}.toOption match {
      case Some(s:(State,Eta)) => true
      case None => false
    }
  }

  def add(id:SlotId,ls:State,eta:Eta):Unit = {
    if (!known(id)) {
      stateStoreCache.get(id._1/epochLength).update(Seq(),Seq(id._2 -> ByteArrayWrapper(serializer.getBytes(ls))))
      etaStoreCache.get(id._1/epochLength).update(Seq(),Seq(id._2 -> ByteArrayWrapper(eta)))
    }
    stateCache.put(id,(ls,eta))
  }

  def get(id:SlotId):Option[(State,Eta)] = Try{stateCache.get(id)}.toOption
}
