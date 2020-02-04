package prosomo

class History extends Types {

  var idMap:Map[Hash,(State,Eta)] = Map()

  def known(id:Hash):Boolean = {idMap.keySet.contains(id)}

  def add(id:Hash,ls:State,eta:Eta) = if (!known(id)) {
    idMap += (id->(ls,eta))
  }

  def get(id:Hash):Any = if (known(id)) {
    idMap(id)
  } else {
    0
  }

  def remove(id:Hash) = if (known(id)) {
    idMap -= id
  }

}
