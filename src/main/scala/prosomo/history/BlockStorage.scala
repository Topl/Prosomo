package prosomo.history

import java.io.File

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Block, Serializer}
import prosomo.primitives.{ByteStream, LDBStore, SharedData, SimpleTypes}

import scala.concurrent.duration.MINUTES

class BlockStorage(dir:String) extends SimpleTypes {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.{storageFlag,cacheSize}

  private val serializer = new Serializer

  private var blockBodyStore:LDBStore = {
    val iFile = new File(s"$dir/blocks/body")
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

  private var blockHeaderStore:LDBStore = {
    val iFile = new File(s"$dir/blocks/header")
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

  private val blockLoader:CacheLoader[SlotId,Block] = new CacheLoader[SlotId,Block] {
    def load(id:SlotId):Block = {
      restore(id._2) match {
        case Some(b:Block) => b
        case None => new Block(id._2,None,None)
      }
    }
  }

  private val blockCache:LoadingCache[SlotId,Block] = CacheBuilder.newBuilder()
    .expireAfterAccess(10,MINUTES).maximumSize(cacheSize)
    .build[SlotId,Block](blockLoader)

  private var data:Map[ByteArrayWrapper,Block] = Map()

  private var slotIds:Map[Slot,Set[BlockId]] = Map()

  def add(block:Block):Unit = if (storageFlag) {
    val blockHeader = block.prosomoHeader
    blockHeaderStore.update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      blockBodyStore.update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getGenesisBytes(
        block.body match {case txs:GenesisSet => {txs}}
      ))))
    } else {
      blockBodyStore.update(Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(
        block.body match {case txs:TransactionSet => txs}
      ))))
    }
    blockCache.put((blockHeader._3,block.id),block)
  } else {
    if (!data.keySet.contains(block.id)) {
      data += (block.id -> block)
      val slot = block.slot
      val slotIdSet:Set[BlockId] = if (slotIds.keySet.contains(slot)) {
        val out = slotIds(slot) ++ Set(block.id)
        slotIds -= slot
        out
      } else {
        Set(block.id)
      }
      slotIds += (slot->slotIdSet)
    }
  }

  def store(key:ByteArrayWrapper,block:Block) = {
    val blockHeader = block.prosomoHeader
    blockHeaderStore.update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      blockBodyStore.update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getGenesisBytes(
        block.body match {case txs:GenesisSet => {txs}}
      ))))
    } else {
      blockBodyStore.update(Seq(),Seq(key -> ByteArrayWrapper(serializer.getBytes(
        block.body match {case txs:TransactionSet => txs}
      ))))
    }
  }

  def restore(key:ByteArrayWrapper):Option[Block] = {
    SharedData.throwDiskWarning
    blockHeaderStore.get(key) match {
      case Some(bytes: ByteArrayWrapper) => {
        val byteStream:ByteStream = new ByteStream(bytes.data,DeserializeBlockHeader)
        serializer.fromBytes(byteStream) match {
          case h:BlockHeader=> {
            blockBodyStore.get(key) match {
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

  def get(id:SlotId):Block = if (storageFlag) {
    blockCache.get(id)
  } else {
    data(id._2)
  }

  def getBody(id:SlotId):Any = if (storageFlag) {
    blockCache.get(id).body
  } else {
    data(id._2).body
  }

  def getBodyData(id:BlockId):Any = blockBodyStore.get(id)

  def getTxs(id:SlotId):TransactionSet = getBody(id) match {
    case txs:TransactionSet => txs
    case _ => Seq()
  }

  def getGenSet(id:SlotId):GenesisSet = getBody(id) match {
    case txs:GenesisSet => txs
    case _ => Seq()
  }

  def known(id:SlotId):Boolean = if (storageFlag) {
    blockCache.getIfPresent(id) match {
      case b:Block => b.header match {
        case h:BlockHeader => true
        case None => false
      }
      case _ => blockHeaderStore.known(id._2)
    }
  } else {
    data.keySet.contains(id._2)
  }

  def known_then_load(id:SlotId):Boolean = if (storageFlag) {
    blockCache.get(id) match {
      case b:Block => b.header match {
        case h:BlockHeader => true
        case None => false
      }
      case _ => false
    }
  } else {
    data.keySet.contains(id._2)
  }

  def slotBlocks(slot:Slot):Map[ByteArrayWrapper,BlockHeader] = {
    var out:Map[ByteArrayWrapper,BlockHeader] = Map()
    if (slotIds.keySet.contains(slot)) for (id <- slotIds(slot)) {out += (id -> get((slot,id)).prosomoHeader)}
    out
  }

  def copy(bd:BlockStorage):Unit = {
    this.data = bd.data
    this.slotIds = bd.slotIds
    this.blockBodyStore = bd.blockBodyStore
    this.blockHeaderStore = bd.blockHeaderStore
  }

}
