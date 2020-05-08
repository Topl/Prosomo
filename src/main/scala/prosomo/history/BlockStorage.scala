package prosomo.history

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache, RemovalListener, RemovalNotification}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Block, Serializer}
import prosomo.primitives.{ByteStream, LDBStore, SharedData, SimpleTypes}

import scala.concurrent.duration.MINUTES

class BlockStorage(dir:String,serializer: Serializer) extends SimpleTypes {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.{cacheSize,epochLength}
  val dbCacheSize = 4
  type DB = LDBStore

  private val headerStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .expireAfterAccess(10,MINUTES)
    .maximumSize(dbCacheSize)
    .removalListener((notification: RemovalNotification[BigInt, DB]) => {
      notification.getValue.close()
    })
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch:BigInt):DB = {
        LDBStore(s"$dir/blocks/header/epoch_$epoch")
      }
    })

  private val bodyStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
    .expireAfterAccess(10,MINUTES)
    .maximumSize(dbCacheSize)
    .removalListener((notification: RemovalNotification[BigInt, DB]) => {
      notification.getValue.close()
    })
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch:BigInt):DB = {
        LDBStore(s"$dir/blocks/body/epoch_$epoch")
      }
    })

  def refresh():Unit = {
    bodyStoreCache.asMap().keySet().forEach(bodyStoreCache.get(_).refresh())
    headerStoreCache.asMap().keySet().forEach(headerStoreCache.get(_).refresh())
  }

  private val blockCache:LoadingCache[SlotId,Block] = CacheBuilder.newBuilder()
    .expireAfterAccess(10,MINUTES).maximumSize(cacheSize)
    .build[SlotId,Block](new CacheLoader[SlotId,Block] {
      def load(id:SlotId):Block = {
        restore(id) match {
          case Some(b:Block) => b
          case None => Block(id._2,None,None)
        }
      }
    })

  def add(block:Block):Unit = {
    val blockHeader = block.prosomoHeader
    headerStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      bodyStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getGenesisBytes(
        block.body match {case txs:GenesisSet => {txs}}
      ))))
    } else {
      bodyStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(
        block.body match {case txs:TransactionSet => txs}
      ))))
    }
    blockCache.put((blockHeader._3,block.id),block)
  }

  def store(key:ByteArrayWrapper,block:Block):Unit = {
    val blockHeader = block.prosomoHeader
    headerStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      bodyStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getGenesisBytes(
        block.body match {case txs:GenesisSet => {txs}}
      ))))
    } else {
      bodyStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getBytes(
        block.body match {case txs:TransactionSet => txs}
      ))))
    }
  }

  def restore(id:SlotId):Option[Block] = {
    val key = id._2
    SharedData.throwDiskWarning
    headerStoreCache.get(id._1/epochLength).get(key) match {
      case Some(bytes: ByteArrayWrapper) => {
        val byteStream:ByteStream = new ByteStream(bytes.data,DeserializeBlockHeader)
        serializer.fromBytes(byteStream) match {
          case h:BlockHeader=> {
            bodyStoreCache.get(id._1/epochLength).get(key) match {
              case Some(bytes:ByteArrayWrapper) => {
                if (h._3 == 0) {
                  val byteStream:ByteStream = new ByteStream(bytes.data,DeserializeGenesisSet)
                  serializer.fromBytes(byteStream) match {
                    case txs:GenesisSet => {
                      val block = Block(key,h,txs)
                      Some(block)
                    }
                    case _ => None
                  }
                } else {
                  val byteStream:ByteStream = new ByteStream(bytes.data,DeserializeTransactionSet)
                  serializer.fromBytes(byteStream) match {
                    case txs:TransactionSet => {
                      val block = Block(key,h,txs)
                      Some(block)
                    }
                    case _ => None
                  }
                }
              }
              case None => {
                val block = Block(key,h,None)
                Some(block)
              }
            }
          }
        }
      }
      case None => None
    }
  }

  def get(id:SlotId):Block = blockCache.get(id)

  def getBody(id:SlotId):Any = blockCache.get(id).body

  def getBodyData(id:SlotId):Any = bodyStoreCache.get(id._1/epochLength).get(id._2)

  def getTxs(id:SlotId):TransactionSet = getBody(id) match {
    case txs:TransactionSet => txs
    case _ => Seq()
  }

  def getGenSet(id:SlotId):GenesisSet = getBody(id) match {
    case txs:GenesisSet => txs
    case _ => Seq()
  }

  def known(id:SlotId):Boolean = {
    blockCache.getIfPresent(id) match {
      case b:Block => b.header match {
        case h:BlockHeader => true
        case None => false
      }
      case _ => headerStoreCache.get(id._1/epochLength).known(id._2)
    }
  }

  def known_then_load(id:SlotId):Boolean = {
    blockCache.get(id) match {
      case b:Block => b.header match {
        case h:BlockHeader => true
        case None => false
      }
      case _ => false
    }
  }

  def slotBlocks(slot:Slot):Map[ByteArrayWrapper,BlockHeader] = {
    var out:Map[ByteArrayWrapper,BlockHeader] = Map()
    out
  }

}
