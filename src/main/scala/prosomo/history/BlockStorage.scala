package prosomo.history

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache, RemovalNotification}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Block, Serializer}
import prosomo.primitives.{ByteStream, LDBStore, SharedData, SimpleTypes}
import scorex.util.encode.Base58
import scala.util.Try

/**
  * AMS 2020:
  * Storage for block body and header data stored separately in databases that are split into thirds of epochs
  */

class BlockStorage(dir:String,serializer: Serializer) extends SimpleTypes {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.{cacheSize,one_third_epoch}
  val dbCacheSize = 4
  type DB = LDBStore

  private val headerStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .maximumSize(dbCacheSize)
    .removalListener((notification: RemovalNotification[BigInt, DB]) => {
      notification.getValue.close()
    })
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch3rd:BigInt):DB = {
        LDBStore(s"$dir/blocks/header/epoch_${epoch3rd/3}_${epoch3rd%3}")
      }
    })

  private val bodyStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .maximumSize(dbCacheSize)
    .removalListener((notification: RemovalNotification[BigInt, DB]) => {
      notification.getValue.close()
    })
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch3rd:BigInt):DB = {
        LDBStore(s"$dir/blocks/body/epoch_${epoch3rd/3}_${epoch3rd%3}")
      }
    })

  def refresh:Unit = {
    bodyStoreCache.asMap().keySet().forEach(bodyStoreCache.get(_).refresh())
    headerStoreCache.asMap().keySet().forEach(headerStoreCache.get(_).refresh())
  }

  private val blockCache:LoadingCache[SlotId,Block] = CacheBuilder.newBuilder()
    .maximumSize(cacheSize)
    .build[SlotId,Block](new CacheLoader[SlotId,Block] {
      def load(id:SlotId):Block = {
        restore(id).get
      }
    })

  def add(block:Block):Unit = {
    val blockHeader = block.prosomoHeader
    headerStoreCache.get(blockHeader._3/one_third_epoch).update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      bodyStoreCache.get(blockHeader._3/one_third_epoch).update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getGenesisBytes(
        block.genesisSet.get
      ))))
    } else {
      bodyStoreCache.get(blockHeader._3/one_third_epoch).update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(
        block.blockBody.get
      ))))
    }
    blockCache.put((blockHeader._3,block.id),block)
  }

  def store(key:ByteArrayWrapper,block:Block):Unit = {
    val blockHeader = block.prosomoHeader
    headerStoreCache.get(blockHeader._3/one_third_epoch).update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      bodyStoreCache.get(blockHeader._3/one_third_epoch).update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getGenesisBytes(
        block.genesisSet.get
      ))))
    } else {
      bodyStoreCache.get(blockHeader._3/one_third_epoch).update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getBytes(
        block.blockBody.get
      ))))
    }
  }

  def restore(id:SlotId):Option[Block] = Try{
    val key = id._2
    SharedData.throwDiskWarning(s"Restore block ${Base58.encode(key.data)}")
    headerStoreCache.get(id._1/one_third_epoch).get(key) match {
      case Some(bytes: ByteArrayWrapper) => {
        val byteStream:ByteStream = new ByteStream(bytes.data,DeserializeBlockHeader)
        serializer.fromBytes(byteStream) match {
          case h:BlockHeader@unchecked => {
            bodyStoreCache.get(id._1/one_third_epoch).get(key) match {
              case Some(bytes:ByteArrayWrapper) => {
                if (h._3 == 0) {
                  val byteStream:ByteStream = new ByteStream(bytes.data,DeserializeGenesisSet)
                  serializer.fromBytes(byteStream) match {
                    case txs:GenesisSet@unchecked => {
                      val block = Block(key,Some(h),None,Some(txs))
                      Some(block)
                    }
                    case _ => None
                  }
                } else {
                  val byteStream:ByteStream = new ByteStream(bytes.data,DeserializeTransactionSet)
                  serializer.fromBytes(byteStream) match {
                    case txs:TransactionSet@unchecked => {
                      val block = Block(key,Some(h),Some(txs),None)
                      Some(block)
                    }
                    case _ => None
                  }
                }
              }
              case None => {
                val block = Block(key,Some(h),None,None)
                Some(block)
              }
            }
          }
        }
      }
      case None => None
    }
  }.toOption match {
    case Some(value) => value
    case None => None
  }

  def get(id:SlotId):Option[Block] = Try{blockCache.get(id)}.toOption

  def getIfPresent(id:SlotId):Option[Block] = {
    Try{
      blockCache.getIfPresent(id) match {
        case b:Block => b
      }
    }.toOption match {
      case Some(b:Block) => Some(b)
      case None => restore(id)
    }
  }

  def known(id:SlotId):Boolean = {
    Try{blockCache.get(id)}.toOption match {
      case Some(b:Block) => true
      case None => false
    }
  }

  def knownIfPresent(id:SlotId):Boolean = {
    Try{
      blockCache.getIfPresent(id) match {
        case b:Block => b
      }
    }.toOption match {
      case Some(b:Block) => true
      case None => {
        headerStoreCache.get(id._1/one_third_epoch).known(id._2)
      }
    }
  }
}
