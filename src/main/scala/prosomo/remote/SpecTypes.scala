package prosomo.remote

import prosomo.primitives.Types._
import prosomo.primitives.Mac
import prosomo.components.{Block,Transaction}

object SpecTypes {
  /*
  used message codes in bifrost:
  1,22,65,55,33,2
   */

  type DiffuseDataType = Option[(String,PublicKeys,Mac)]
  type HelloDataType = Option[(String,Mac)]
  type RequestBlockType = Option[(SlotId,Mac,Int)]
  type RequestBlocksType = Option[(SlotId,Int,Mac,Int)]
  type ReturnBlocksType = Option[(List[Block],Mac,Int)]
  type SendBlockType = Option[(Block,Mac)]
  type SendTxType = Option[Transaction]

  val diffuseCode:Byte = 100:Byte
  val helloCode:Byte = 101:Byte
  val requestBlockCode:Byte = 102:Byte
  val requestBlocksCode:Byte = 103:Byte
  val returnBlocksCode:Byte = 104:Byte
  val sendBlockCode:Byte = 105:Byte
  val sendTxCode:Byte = 106:Byte

}
