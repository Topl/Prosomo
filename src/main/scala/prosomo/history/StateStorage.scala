package prosomo.history

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache, RemovalNotification}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.Serializer
import prosomo.primitives.{ByteStream, Fch, LDBStore, SharedData, Types, Parameters}
import scorex.util.encode.Base58
import scala.util.Try

/**
  * AMS 2020:
  * The state and epoch nonce are stored in separate databases split into thirds of epochs
  */

class StateStorage(dir:String,serializer:Serializer) extends Types {
  import prosomo.components.Serializer._
  val cacheSize = Parameters.cacheSize
  val one_ninth_epoch = Parameters.one_ninth_epoch
  val dbCacheSize = 4
  type DB = LDBStore
  val fch:Fch = new Fch

  private val stateStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .maximumSize(dbCacheSize)
    .removalListener((notification: RemovalNotification[BigInt, DB]) => {
      notification.getValue.close()
    })
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch9th:BigInt):DB = {
        LDBStore(s"$dir/history/state/epoch_${epoch9th/9}_${epoch9th%9}")
      }
    })

  private val etaStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .maximumSize(dbCacheSize)
    .removalListener((notification: RemovalNotification[BigInt, DB]) => {
      notification.getValue.close()
    })
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch9th:BigInt):DB = {
        LDBStore(s"$dir/history/eta/epoch_${epoch9th/9}_${epoch9th%9}")
      }
    })

  def refresh():Unit = {
    etaStoreCache.asMap().keySet().forEach(etaStoreCache.get(_).refresh())
    stateStoreCache.asMap().keySet().forEach(stateStoreCache.get(_).refresh())
  }

  private val epochStakeDistCache:LoadingCache[SlotId,(StateData,Eta)] = CacheBuilder.newBuilder()
    .maximumSize(4)
    .build[SlotId,(StateData,Eta)](new CacheLoader[SlotId,(StateData,Eta)] {
      def load(id:SlotId):(StateData,Eta) = {
        stateCache.get(id)
      }
    })

  def getStakeDist(id:SlotId):StateData = {
    epochStakeDistCache.get(id)._1
  }

  def cacheStakeDist(id:SlotId):Unit = epochStakeDistCache.refresh(id)

  private val stateCache:LoadingCache[SlotId,(StateData,Eta)] = CacheBuilder.newBuilder()
    .maximumSize(cacheSize)
    .build[SlotId,(StateData,Eta)](new CacheLoader[SlotId,(StateData,Eta)] {
      def load(id:SlotId):(StateData,Eta) = {
        SharedData.throwDiskWarning(s"state database ${Base58.encode(id._2.data)}")
        (
          stateStoreCache.get(id._1/one_ninth_epoch).get(id._2).get match {
            case bytes:ByteArrayWrapper => {
              val byteStream = new ByteStream(bytes.data,DeserializeState)
              serializer.fromBytes(byteStream) match {
                case s:StateData@unchecked => s
              }
            }
          },
          etaStoreCache.get(id._1/one_ninth_epoch).get(id._2).get match {
            case bytes:ByteArrayWrapper => bytes.data
          }
        )
      }
    })

  def known(id:SlotId):Boolean = {
    Try{stateCache.getIfPresent(id)}.toOption match {
      case Some(s:(StateData,Eta)) => true
      case _ => stateStoreCache.get(id._1/one_ninth_epoch).known(id._2)
    }
  }

  def known_then_load(id:SlotId):Boolean = {
    Try{stateCache.get(id)}.toOption match {
      case Some(s:(StateData,Eta)) => true
      case None => false
    }
  }

  def add(id:SlotId, ls:StateData, eta:Eta):Unit = {
    if (!known(id)) {
      stateStoreCache.get(id._1/one_ninth_epoch).update(Seq(),Seq(id._2 -> ByteArrayWrapper(serializer.getBytes(ls))))
      etaStoreCache.get(id._1/one_ninth_epoch).update(Seq(),Seq(id._2 -> ByteArrayWrapper(eta)))
    }
    stateCache.put(id,(ls,eta))
  }

  def get(id:SlotId):Option[(StateData,Eta)] = Try{stateCache.get(id)}.toOption
}
