package prosomo.components

import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives.{Mac, SimpleTypes}
import prosomo.primitives.Types.{BlockHeader,GenesisSet,TransactionSet}

/*
  Block container with optional fields and a unique identifier
 */

case class Block(identifier:ByteArrayWrapper,
                 blockHeader:Option[BlockHeader],
                 blockBody:Option[TransactionSet],
                 genesisSet: Option[GenesisSet]
                ) extends SimpleTypes {
  def id:BlockId = identifier
  def slotId:SlotId = (slot,id)
  def parentSlotId:SlotId = {
    val header = blockHeader.get
    (header._10,header._1)
  }
  def prosomoHeader:BlockHeader = blockHeader.get
  def pid:BlockId = prosomoHeader._1
  def ledger:Mac = prosomoHeader._2
  def slot:Slot = prosomoHeader._3
  def certificate:Cert = prosomoHeader._4
  def nonce:Rho = prosomoHeader._5
  def proof:Pi = prosomoHeader._6
  def signature:MalkinSignature = prosomoHeader._7
  def kes_key:PublicKey = prosomoHeader._8
  def number:BlockNumber = prosomoHeader._9
  def parentSlot:Slot = prosomoHeader._10
}
