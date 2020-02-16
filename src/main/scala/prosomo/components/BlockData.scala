package prosomo.components

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

class BlockData(dir:String) extends SimpleTypes {

  val blockBodyStore:LSMStore = {
    val iFile = new File(s"$dir/blocks/body")
    iFile.mkdirs()
    val store = new LSMStore(iFile)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        store.close()
      }
    })
    store
  }

  val blockHeaderStore:LSMStore = {
    val iFile = new File(s"$dir/blocks/header")
    iFile.mkdirs()
    val store = new LSMStore(iFile)
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        store.close()
      }
    })
    store
  }

  private var data:Map[ByteArrayWrapper,Block] = Map()

  private var slotIds:Map[Slot,Set[BlockId]] = Map()

  def add(block:Block):Unit = {
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

  def get(id:BlockId):Block = data(id)
  def get(id:SlotId):Block = data(id._2)

  def getBody(id:BlockId):Any = data(id).body
  def getBody(id:SlotId):Any = data(id._2).body

  def getTxs(id:BlockId):TransactionSet = getBody(id) match {case txs:TransactionSet => txs}
  def getTxs(id:SlotId):TransactionSet = getBody(id) match {case txs:TransactionSet => txs}

  def getGenSet(id:BlockId):GenesisSet = getBody(id) match {case txs:GenesisSet => txs}
  def getGenSet(id:SlotId):GenesisSet = getBody(id) match {case txs:GenesisSet => txs}

  def known(id:BlockId):Boolean = data.keySet.contains(id)
  def known(id:SlotId):Boolean = data.keySet.contains(id._2)

  def slotBlocks(slot:Slot):Map[ByteArrayWrapper,BlockHeader] = {
    var out:Map[ByteArrayWrapper,BlockHeader] = Map()
    if (slotIds.keySet.contains(slot)) for (id <- slotIds(slot)) {out += (id -> get(id).prosomoHeader)}
    out
  }

  def delete(id:BlockId):Unit = {
    if (data.keySet.contains(id)) {
      val slot = get(id).slot
      val slotIdSet:Set[BlockId] = {
        val out = slotIds(slot) -- Set(id)
        slotIds -= slot
        out
      }
      slotIds += (slot->slotIdSet)
      data -= id
    }
  }

}
