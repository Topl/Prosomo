package prosomo.components

import io.iohk.iodb.ByteArrayWrapper

class Block(identifier:ByteArrayWrapper, blockHeader:Any, blockBody:Any) extends SimpleTypes {
  def id:BlockId = identifier
  def data:Any = blockHeader
  def header:BlockHeader = blockHeader match {
    case h:BlockHeader => h
  }
  def pid:BlockId = header._1
  def ledger:Ledger = header._2
  def slot:Slot = header._3
  def certificate:Cert = header._4
  def nonce:Rho = header._5
  def proof:Pi = header._6
  def signature:KesSignature = header._7
  def kes_key:PublicKey = header._8
  def number:BlockNumber = header._9
  def parentSlot:Slot = header._10
}
