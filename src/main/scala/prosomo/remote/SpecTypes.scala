package prosomo.remote

import prosomo.primitives.Types._
import prosomo.primitives.Mac
import prosomo.components.{Block,Transaction}

object SpecTypes {
  /*
  used message codes in bifrost:
  1,22,65,55,33,2
   */

  type DiffuseDataType = (String,String,PublicKeys,Mac)
  type HelloDataType = (String,String,Mac)
  type RequestBlockType = (String,String,SlotId,Mac,Int)
  type RequestTineType = (String,String,SlotId,Int,Mac,Int)
  type ReturnBlocksType = (String,String,List[Block],Mac,Int)
  type SendBlockType = (String,String,Block,Mac)
  type SendTxType = (String,String,Transaction)

  val diffuseCode:Byte = 100:Byte
  val helloCode:Byte = 101:Byte
  val requestBlockCode:Byte = 102:Byte
  val requestTineCode:Byte = 103:Byte
  val returnBlocksCode:Byte = 104:Byte
  val sendBlockCode:Byte = 105:Byte
  val sendTxCode:Byte = 106:Byte
  val holdersFromRemote:Byte = 107:Byte

}
