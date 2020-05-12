package prosomo.primitives

import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.Transaction

import scala.math.BigInt

trait SimpleTypes {
  val pk_length = 32
  val pkw_length = pk_length*3
  val sig_length = 64
  val hash_length = 32
  val eta_length = 32
  val int_length = 4
  val long_length = 8
  val sid_length = 32
  val pi_length = 80
  val rho_length = 64
  val id_length = hash_length
  val mac_length = 2*hash_length + sig_length + pk_length
  val slot_length = int_length
  val bn_length = int_length
  val sk_length = 32

  val localChainId = ByteArrayWrapper(FastCryptographicHash("LOCAL_CHAIN"))

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
  type TransactionSet = Seq[Transaction]
  type GenesisSet = Seq[(Array[Byte], ByteArrayWrapper, BigInt,Mac)]
  type KesSignature = (Array[Byte],Array[Byte],Array[Byte])
  type BlockHeader = (Hash,Mac,Slot,Cert,Rho,Pi,KesSignature,PublicKey,BlockNumber,Slot)
  type Request = (List[SlotId],Int,Int)
  type State = Map[PublicKeyW,(BigInt,Boolean,Int)]
  type MemPool = Map[Sid,(Transaction,Int)]
}
