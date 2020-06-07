package prosomo.history

import prosomo.primitives.{ByteStream, Fch, LDBStore, Types}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.Serializer

/**
  * AMS 2020:
  * Storage for tine structure information and research
  */


class SlotHistoryStorage(dir:String) extends Types {
  import prosomo.components.Serializer._
  override val fch = new Fch
  val storageFlag = false
  val skipUpdate = true

  private var data:Map[Slot,List[BlockId]] = Map()

  private var blockReorgStore:LDBStore = LDBStore(s"$dir/history/reorg")

  def uuid = ByteArrayWrapper(fch.hash(java.util.UUID.randomUUID.toString))

  def update(slotId:SlotId,serializer: Serializer):Unit = if (storageFlag) {
    val blockSlotHash = hash(slotId._1,serializer)
    val slotList = get(blockSlotHash,serializer)
    blockReorgStore.update(Seq(),Seq(blockSlotHash -> ByteArrayWrapper(serializer.getBytes(slotId._2::slotList))))
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
        val byteStream = new ByteStream(bytes.data,DeserializeIdList)
        serializer.fromBytes(byteStream) match {case idl:List[BlockId]@unchecked => idl}
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
        val byteStream = new ByteStream(bytes.data,DeserializeIdList)
        serializer.fromBytes(byteStream) match {case idl:List[BlockId]@unchecked => idl}
      }
      case _ => List(ByteArrayWrapper(Array()))
    }
  }

  def copy(srh:SlotHistoryStorage):Unit = {
    this.data = srh.data
    this.blockReorgStore = srh.blockReorgStore
  }

}
