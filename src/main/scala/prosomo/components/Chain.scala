package prosomo.components

import io.iohk.iodb.ByteArrayWrapper
import scala.collection.immutable.ListMap

class Chain extends SimpleTypes {

  private var data:Map[Slot,BlockId] = Map()

  def update(slotId:SlotId):Unit = if (!data.keySet.contains(slotId._1) && slotId._1 > -1) data += (slotId._1 -> slotId._2)

  def remove(slot:Slot):Unit = if (data.keySet.contains(slot)) data -= slot

  def get(slot:Slot):SlotId = if (data.keySet.contains(slot)) {
    (slot,data(slot))
  } else {
    (-1,ByteArrayWrapper(Array()))
  }

  def last:SlotId = {
    val maxSlot = data.keySet.max
    (maxSlot,data(maxSlot))
  }

  def head:SlotId = {
    val minSlot = data.keySet.min
    (minSlot,data(minSlot))
  }

  def slice(minSlot:Slot,maxSlot:Slot):Chain = {
    val out:Chain = new Chain
    for (slot <- data.keySet) {
      if(slot >= minSlot && slot < maxSlot) out.update((slot,data(slot)))
    }
    out
  }

  def slots:Set[Slot] = if (data.isEmpty) {Set(-1)} else {data.keySet}

  def ordered:Array[SlotId] = {
    ListMap(data.toSeq.sortBy(_._1):_*).toArray
  }

  private def copy(c:Chain):Unit = {
    for (slot <- c.slots) {
      update(c.get(slot))
    }
  }

  def isEmpty:Boolean = {
    data.keySet.isEmpty
  }

  def length:Int = {
    data.keySet.size
  }

  def ++(that:Chain):Chain = {
    val out:Chain = new Chain
    val minThis = this.slots.min
    val minThat = that.slots.min
    if (minThis < minThat) {
      out.copy(this.slice(minThis,minThat))
    }
    out.copy(that)
    out
  }

}

object Chain extends SimpleTypes {

  def apply():Chain = new Chain

  def apply(id:SlotId):Chain = {
    val out = new Chain
    out.update(id)
    out
  }

  def apply(ids:Array[SlotId]):Chain = {
    val out = new Chain
    for (id<-ids) out.update(id)
    out
  }

  def apply(c:Chain): Chain = {
    val out = new Chain
    out.copy(c)
    out
  }
}
