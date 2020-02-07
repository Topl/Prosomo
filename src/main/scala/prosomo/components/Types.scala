package prosomo.components

import io.iohk.iodb.ByteArrayWrapper

import scala.math.BigInt

trait Types extends SimpleTypes {
  type Box = (Any,Sid,Signature,PublicKey)
  type Transaction = (PublicKeyW,PublicKeyW,BigInt,Sid,Int,Signature)
  type ChainRequest = (SlotId,Int,Int)
  type BlockRequest = (SlotId,Int)
  type State = Map[PublicKeyW,(BigInt,Boolean,Int)]
  type MemPool = Map[Sid,(Transaction,Int)]
  type Tine = Array[SlotId]
  type ReorgHistory = Array[List[SlotId]]
}

