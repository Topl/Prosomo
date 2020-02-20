package prosomo.components

import java.io.File

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

class BlockData(dir:String) extends SimpleTypes {
  import prosomo.primitives.Parameters.storageFlag
  import prosomo.components.Serializer._

  private var blockBodyStore:LSMStore = {
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

  private var blockHeaderStore:LSMStore = {
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

  def add(block:Block,serializer: Serializer):Unit = if (storageFlag) {
    val blockHeader = block.prosomoHeader
    blockHeaderStore.update(block.id,Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      blockBodyStore.update(block.id,Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getGenesisBytes(
        block.body match {case txs:GenesisSet => {txs}}
      ))))
    } else {
      blockBodyStore.update(block.id,Seq(),Seq(block.id -> ByteArrayWrapper(serializer.getBytes(
        block.body match {case txs:TransactionSet => txs}
      ))))
    }
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

  def get(id:BlockId,serializer: Serializer):Block = if (storageFlag) {
    val blockHeader = blockHeaderStore.get(id) match {
      case Some(bytes: ByteArrayWrapper) => serializer.fromBytes(new ByteStream(bytes.data,DeserializeBlockHeader)) match {case h:BlockHeader=>h}
    }
    val blockBody = if (blockHeader._3 == 0) {
      getGenBody(id,serializer)
    } else {
      getBody(id,serializer)
    }
    new Block(id,blockHeader,blockBody)
  } else {
    data(id)
  }
  def get(id:SlotId,serializer: Serializer):Block = get(id._2,serializer)

  def getBody(id:BlockId,serializer: Serializer):Any = if (storageFlag) {
    blockBodyStore.get(id) match {
      case Some(bytes: ByteArrayWrapper) => serializer.fromBytes(new ByteStream(bytes.data,DeserializeTransactionSet))
      case None => None
    }
  } else {
    data(id).body
  }
  def getBody(id:SlotId,serializer: Serializer):Any = getBody(id._2,serializer)

  def getGenBody(id:BlockId,serializer: Serializer):Any = if (storageFlag) {
    blockBodyStore.get(id) match {
      case Some(bytes: ByteArrayWrapper) => serializer.fromBytes(new ByteStream(bytes.data,DeserializeGenesisSet))
      case None => None
    }
  } else {
    data(id).body
  }
  def getGenBody(id:SlotId,serializer: Serializer):Any = getGenBody(id._2,serializer)

  def getTxs(id:BlockId,serializer: Serializer):TransactionSet = getBody(id,serializer) match {case txs:TransactionSet => txs}
  def getTxs(id:SlotId,serializer: Serializer):TransactionSet = getBody(id,serializer) match {case txs:TransactionSet => txs}

  def getGenSet(id:BlockId,serializer: Serializer):GenesisSet = getGenBody(id,serializer) match {case txs:GenesisSet => txs}
  def getGenSet(id:SlotId,serializer: Serializer):GenesisSet = getGenBody(id,serializer) match {case txs:GenesisSet => txs}

  def known(id:BlockId):Boolean = if (storageFlag) {
    checkHeaderStore(id)
  } else {
    data.keySet.contains(id)
  }

  def known(id:SlotId):Boolean = if (storageFlag) {
    checkHeaderStore(id._2)
  } else {
    data.keySet.contains(id._2)
  }

  def checkHeaderStore(id:BlockId):Boolean = {
    blockHeaderStore.versionIDExists(id)
  }

  def slotBlocks(slot:Slot,serializer: Serializer):Map[ByteArrayWrapper,BlockHeader] = {
    var out:Map[ByteArrayWrapper,BlockHeader] = Map()
    if (slotIds.keySet.contains(slot)) for (id <- slotIds(slot)) {out += (id -> get(id,serializer).prosomoHeader)}
    out
  }

  def delete(id:BlockId,serializer: Serializer):Unit = if(storageFlag) {
    blockHeaderStore
  } else {
    if (data.keySet.contains(id)) {
      val slot = get(id,serializer).slot
      val slotIdSet:Set[BlockId] = {
        val out = slotIds(slot) -- Set(id)
        slotIds -= slot
        out
      }
      slotIds += (slot->slotIdSet)
      data -= id
    }
  }

  def copy(bd:BlockData):Unit = {
    this.data = bd.data
    this.slotIds = bd.slotIds
    this.blockBodyStore = bd.blockBodyStore
    this.blockHeaderStore = bd.blockHeaderStore
  }

}
