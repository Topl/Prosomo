package prosomo.components

import prosomo.primitives.Ratio
import io.iohk.iodb.ByteArrayWrapper

import scala.math.BigInt

trait SimpleTypes {
  type Hash = ByteArrayWrapper
  type Eta = Array[Byte]
  type Signature = Array[Byte]
  type Slot = Int
  type BlockNumber = Int
  type Rho = Array[Byte]
  type PublicKey = Array[Byte]
  type Sid = Hash
  type PublicKeyW = ByteArrayWrapper
  type PublicKeys = (PublicKey,PublicKey,PublicKey)
  type PrivateKey = Array[Byte]
  type Pi = Array[Byte]
  type BlockId = Hash
  type SlotId = (Slot,BlockId)
  type Cert = (PublicKey,Rho,Pi,PublicKey,Ratio,String)
  type Ledger = List[Any]
  type KesSignature = (Array[Byte],Array[Byte],Array[Byte])
  type BlockHeader = (Hash,Ledger,Slot,Cert,Rho,Pi,KesSignature,PublicKey,BlockNumber,Slot)
  type ChainRequest = (SlotId,Int,Int)
  type BlockRequest = (SlotId,Int)
  type State = Map[PublicKeyW,(BigInt,Boolean,Int)]
  type MemPool = Map[Sid,(Transaction,Int)]
}
