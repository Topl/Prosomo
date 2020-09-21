package prosomo.components

import prosomo.primitives.Types.{StateData,StateKey,StateValue}

case class State(data:StateData) {

  var pending:Seq[(StateKey,Option[StateValue])] = Seq()

  def += (kv:(StateKey,StateValue)):Unit = {
    pending ++= Seq((kv._1,Some(kv._2)))
  }

  def -= (k:StateKey):Unit = {
    pending ++= Seq(k,None)
  }

  def get(k:StateKey):Option[StateValue] = {
    pending.reverse.find(_._1 == k) match {
      case Some(kv) => kv._2
      case None => data.get(k)
    }
  }

  def commit():Unit = {
    pending.foreach( entry =>
      entry._2 match {
        case Some(v) => data.update(entry._1,v)
        case None => data.remove(entry._1)
      }
    )
    pending = Seq()
  }

  def purge():Unit =
    pending = Seq()

  def copy:State = {
    val newState = State(data = this.data)
    newState.pending = this.pending
    newState
  }

}