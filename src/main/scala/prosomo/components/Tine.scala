package prosomo.components

import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives.SimpleTypes
import scorex.crypto.encode.Base58
import prosomo.primitives.Types._

import scala.collection.immutable.ListMap

case class Tine(var data:Map[Slot,(BlockId,Rho)] = Map()) {

  def update(slotId:SlotId,nonce:Rho):Unit = if (!data.keySet.contains(slotId._1) && slotId._1 > -1) {
    val newEntry:(BlockId,Rho) = (slotId._2,nonce)
    data += (slotId._1 -> newEntry)
  }

  def remove(slot:Slot):Unit = if (data.keySet.contains(slot)) data -= slot

  def get(slot:Slot):SlotId = if (data.keySet.contains(slot)) {
    (slot,data(slot)._1)
  } else {
    (-1,ByteArrayWrapper(Array()))
  }

  def getNonce(slot:Slot):Rho = if (data.keySet.contains(slot)) {
    data(slot)._2
  } else {
    Array()
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
    (maxSlot,data(maxSlot)._1)
  }

  def least:SlotId = {
    val minSlot = data.keySet.min
    (minSlot,data(minSlot)._1)
  }

  def slice(minSlot:Slot,maxSlot:Slot):Tine = {
    val out:Tine = new Tine
    for (slot <- data.keySet) {
      if(slot >= minSlot && slot < maxSlot) out.update((slot,data(slot)._1),data(slot)._2)
    }
    out
  }

  def slots:Set[Slot] = if (data.isEmpty) {Set(-1)} else {data.keySet}

  def toSlotId(data:(Slot,(BlockId,Rho))):SlotId = (data._1,data._2._1)

  def ordered:Array[SlotId] = {
    ListMap(data.toSeq.sortBy(_._1):_*).toArray.map(toSlotId)
  }

  def copy(c:Tine):Unit = {
    for (slot <- c.slots) {
      update(c.get(slot),c.getNonce(slot))
    }
  }

  private def setData(ids:Map[Slot,(BlockId,Rho)]):Unit = {
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

  def apply(id:SlotId,nonce:Rho):Tine = {
    val out = new Tine
    out.update(id,nonce)
    out
  }

  def apply(ids:Map[Slot,(BlockId,Rho)]):Tine = {
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
