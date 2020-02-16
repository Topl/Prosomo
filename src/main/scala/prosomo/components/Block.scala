package prosomo.components

import io.iohk.iodb.ByteArrayWrapper

case class Block(identifier:ByteArrayWrapper, blockHeader:Any, blockBody:Any) extends SimpleTypes {
  def id:BlockId = identifier
  def body:Any = blockBody
  def header:Any = blockHeader
  def prosomoHeader:BlockHeader = blockHeader match {
    case h:BlockHeader => h
  }
  def pid:BlockId = prosomoHeader._1
  def ledger:Box = prosomoHeader._2
  def slot:Slot = prosomoHeader._3
  def certificate:Cert = prosomoHeader._4
  def nonce:Rho = prosomoHeader._5
  def proof:Pi = prosomoHeader._6
  def signature:KesSignature = prosomoHeader._7
  def kes_key:PublicKey = prosomoHeader._8
  def number:BlockNumber = prosomoHeader._9
  def parentSlot:Slot = prosomoHeader._10
}
