package prosomo.history

import java.io.File

import bifrost.crypto.hash.FastCryptographicHash
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import prosomo.components.Serializer
import prosomo.primitives.{ByteStream, Types}

class SlotHistoryStorage(dir:String) extends Types {
  import prosomo.components.Serializer._

  //import prosomo.primitives.Parameters.storageFlag
  val storageFlag = false
  val skipUpdate = true

  private var data:Map[Slot,List[BlockId]] = Map()

  private var blockReorgStore:LSMStore = {
    val iFile = new File(s"$dir/history/reorg")
    iFile.mkdirs()
    val store = new LSMStore(iFile)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        store.close()
      }
    })
    store
  }

  def uuid = ByteArrayWrapper(FastCryptographicHash(java.util.UUID.randomUUID.toString))

  def update(slotId:SlotId,serializer: Serializer):Unit = if (storageFlag) {
    val blockSlotHash = hash(slotId._1,serializer)
    val slotList = get(blockSlotHash,serializer)
    blockReorgStore.update(uuid,Seq(),Seq(blockSlotHash -> ByteArrayWrapper(serializer.getBytes(slotId._2::slotList))))
  } else if (!skipUpdate) {
    if (data.keySet.contains(slotId._1)) {
      val newList = slotId._2::data(slotId._1)
      data -= slotId._1
      data += (slotId._1 -> newList)
    } else {
      val newList = slotId._2::List(ByteArrayWrapper(Array()))
      data += (slotId._1 -> newList)
    }
  }

  def get(slot:Slot,serializer: Serializer):List[BlockId] = if (storageFlag) {
    val blockSlotHash = hash(slot,serializer)
    blockReorgStore.get(blockSlotHash) match {
      case Some(bytes: ByteArrayWrapper) => {
        serializer.fromBytes(new ByteStream(bytes.data,DeserializeIdList)) match {case idl:List[BlockId] => idl}
      }
      case None => List(ByteArrayWrapper(Array()))
    }
  } else {
    if (data.keySet.contains(slot)) {
      data(slot)
    } else {
      List(ByteArrayWrapper(Array()))
    }
  }

  private def get(blockSlotHash:Hash,serializer: Serializer):List[BlockId] = {
    blockReorgStore.get(blockSlotHash) match {
      case Some(bytes: ByteArrayWrapper) => {
        serializer.fromBytes(new ByteStream(bytes.data,DeserializeIdList)) match {case idl:List[BlockId] => idl}
      }
      case _ => List(ByteArrayWrapper(Array()))
    }
  }

  def copy(srh:SlotHistoryStorage):Unit = {
    this.data = srh.data
    this.blockReorgStore = srh.blockReorgStore
  }

}
