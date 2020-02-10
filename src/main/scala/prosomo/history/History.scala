package prosomo.history

import prosomo.components.Types
import scorex.crypto.encode.Base58

class History extends Types {

  var idMap:Map[Hash,(State,Eta)] = Map()

  def known(id:BlockId):Boolean = {idMap.keySet.contains(id)}

  def known(id:SlotId):Boolean = {idMap.keySet.contains(id._2)}

  def add(id:Hash,ls:State,eta:Eta) = if (!known(id)) {
    idMap += (id->(ls,eta))
  }

  def get(id:Hash):Any = if (known(id)) {
    idMap(id)
  } else {
    println("Warning: Unknown id in history "+Base58.encode(id.data))
    0
  }

  def remove(id:Hash) = if (known(id)) {
    idMap -= id
  }

}
