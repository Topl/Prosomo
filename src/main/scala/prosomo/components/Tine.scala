package prosomo.components

import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives.SimpleTypes
import scorex.crypto.encode.Base58
import prosomo.primitives.Types._

import scala.collection.immutable.ListMap

case class Tine(var data:Map[Slot,BlockId] = Map()) {

  def update(slotId:SlotId):Unit = if (!data.keySet.contains(slotId._1) && slotId._1 > -1) data += (slotId._1 -> slotId._2)

  def remove(slot:Slot):Unit = if (data.keySet.contains(slot)) data -= slot

  def get(slot:Slot):SlotId = if (data.keySet.contains(slot)) {
    (slot,data(slot))
  } else {
    (-1,ByteArrayWrapper(Array()))
  }

  def getData = data.toSeq

  def getLastActiveSlot(slot:Slot):SlotId = get(lastActiveSlot(slot))

  def lastActiveSlot(s:Slot): Slot = {
    var i:Slot = -1
    for (slot <- this.slots) {
      if (slot > i && slot <= s) i = slot
    }
    i
  }

  def last:SlotId = {
    val maxSlot = data.keySet.max
    (maxSlot,data(maxSlot))
  }

  def head:SlotId = {
    val minSlot = data.keySet.min
    (minSlot,data(minSlot))
  }

  def slice(minSlot:Slot,maxSlot:Slot):Tine = {
    val out:Tine = new Tine
    for (slot <- data.keySet) {
      if(slot >= minSlot && slot < maxSlot) out.update((slot,data(slot)))
    }
    out
  }

  def slots:Set[Slot] = if (data.isEmpty) {Set(-1)} else {data.keySet}

  def ordered:Array[SlotId] = {
    ListMap(data.toSeq.sortBy(_._1):_*).toArray
  }

  def copy(c:Tine):Unit = {
    for (slot <- c.slots) {
      update(c.get(slot))
    }
  }

  private def setData(ids:Map[Slot,BlockId]):Unit = {
    data = ids
  }

  def isEmpty:Boolean = {
    data.keySet.isEmpty
  }

  def length:Int = {
    data.keySet.size
  }

  def print:Unit = {
    for (id<-this.ordered) {
      if (id._1 > -1) println("Slot:"+id._1.toString+" id:"+Base58.encode(id._2.data))
    }
  }

  def ++(that:Tine):Tine = {
    val out:Tine = new Tine
    val minThis = this.slots.min
    val minThat = that.slots.min
    if (minThis < minThat) {
      out.copy(this.slice(minThis,minThat))
    }
    out.copy(that)
    out
  }

}

object Tine extends SimpleTypes {

  def apply():Tine = new Tine

  def apply(id:SlotId):Tine = {
    val out = new Tine
    out.update(id)
    out
  }

  def apply(ids:Array[SlotId]):Tine = {
    val out = new Tine
    for (id<-ids) out.update(id)
    out
  }

  def apply(ids:Map[Slot,BlockId]):Tine = {
    val out = new Tine
    out.setData(ids)
    out
  }

  def apply(c:Tine): Tine = {
    val out = new Tine
    out.copy(c)
    out
  }
}
