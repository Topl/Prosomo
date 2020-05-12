package prosomo.history

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache, RemovalNotification}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Block, Serializer}
import prosomo.primitives.{ByteStream, LDBStore, SharedData, SimpleTypes}

class BlockStorage(dir:String,serializer: Serializer) extends SimpleTypes {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.{cacheSize,epochLength}
  val dbCacheSize = 4
  type DB = LDBStore

  private val headerStoreCache:LoadingCache[BigInt,DB] = CacheBuilder.newBuilder()
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
    .maximumSize(dbCacheSize)
    .removalListener((notification: RemovalNotification[BigInt, DB]) => {
      notification.getValue.close()
    })
    .build[BigInt,DB](new CacheLoader[BigInt,DB] {
      def load(epoch:BigInt):DB = {
        LDBStore(s"$dir/blocks/body/epoch_$epoch")
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
        restore(id) match {
          case Some(b:Block) => b
          case None => Block(id._2,None,None,None)
        }
      }
    })

  def add(block:Block):Unit = {
    val blockHeader = block.prosomoHeader
    headerStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      bodyStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getGenesisBytes(
        block.genesisSet.get
      ))))
    } else {
      bodyStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(
        block.blockBody.get
      ))))
    }
    blockCache.put((blockHeader._3,block.id),block)
  }

  def store(key:ByteArrayWrapper,block:Block):Unit = {
    val blockHeader = block.prosomoHeader
    headerStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      bodyStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getGenesisBytes(
        block.genesisSet.get
      ))))
    } else {
      bodyStoreCache.get(blockHeader._3/epochLength).update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getBytes(
        block.blockBody.get
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
          case h:BlockHeader@unchecked => {
            bodyStoreCache.get(id._1/epochLength).get(key) match {
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
  }

  def get(id:SlotId):Block = blockCache.get(id)

  def getBody(id:SlotId):Option[TransactionSet] = blockCache.get(id).blockBody

  def getGenBody(id:SlotId):Option[GenesisSet] = blockCache.get(id).genesisSet

  def getTxs(id:SlotId):TransactionSet = getBody(id) match {
    case Some(txs:TransactionSet) => txs
  }

  def getGenSet(id:SlotId):GenesisSet = getGenBody(id) match {
    case Some(txs:GenesisSet) => txs
  }

  def known(id:SlotId):Boolean = {
    blockCache.getIfPresent(id) match {
      case b:Block => b.blockHeader match {
        case None => false
        case Some(h:BlockHeader) => true
      }
      case _ => headerStoreCache.get(id._1/epochLength).known(id._2)
    }
  }

  def known_then_load(id:SlotId):Boolean = {
    blockCache.get(id) match {
      case b:Block => b.blockHeader match {
        case None => false
        case Some(h:BlockHeader) => true
      }
      case _ => false
    }
  }

  def slotBlocks(slot:Slot):Map[ByteArrayWrapper,BlockHeader] = {
    var out:Map[ByteArrayWrapper,BlockHeader] = Map()
    out
  }

}
