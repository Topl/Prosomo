package prosomo.components

import io.iohk.iodb.ByteArrayWrapper

class SlotReorgHistory extends SimpleTypes {

  private var data:Map[Slot,List[BlockId]] = Map()

  def update(slotId:SlotId):Unit = {
    if (data.keySet.contains(slotId._1)) {
      val newList = slotId._2::data(slotId._1)
      data -= slotId._1
      data += (slotId._1 -> newList)
    } else {
      val newList = slotId._2::List(ByteArrayWrapper(Array()))
      data += (slotId._1 -> newList)
    }
  }

  def get(slot:Slot):List[BlockId] = {
    if (data.keySet.contains(slot)) {
      data(slot)
    } else {
      List(ByteArrayWrapper(Array()))
    }
  }

}
