package prosomo.stakeholder

import prosomo.components.Tine

trait Operations extends Members {

  /**
    * retrieve parent block id from block
    * @param b
    * @return parent id
    */
  def getParentId(b:BlockHeader): SlotId = {
    (b._10,b._1)
  }

  /**
    * finds the last non-empty slot in a chain
    * @param c chain of block ids
    * @param s slot to start search
    * @return last active slot found on chain c starting at slot s
    */
  def lastActiveSlot(c:Tine, s:Slot): Slot = {
    var i:Slot = -1
    for (slot <- c.slots) {
      if (slot > i && slot <= s) i = slot
    }
    i
  }

  /**
    * returns the total number of active slots on a chain
    * @param c chain of block ids
    * @return total active slots
    */
  def getActiveSlots(c:Tine): Int = {
    c.slots.size
  }

  /**
    * returns a sub-chain containing all blocks in a given time interval
    * @param c input chain
    * @param t1 slot lower bound
    * @param t2 slot upper bound
    * @return all blocks in the interval t1 to t2, including blocks of t1 and t2
    */

  def subChain(c:Tine, t1:Int, t2:Int): Tine = {
    var t_lower:Int = 0
    var t_upper:Int = 0
    if (t1>0) t_lower = t1
    if (t2>0) t_upper = t2
    c.slice(t_lower,t_upper+1)
  }


  /**
    * retrieve a block header from database
    * @param bid
    * @return block if found, 0 otherwise
    */
  def getBlockHeader(bid:SlotId): Option[BlockHeader] = {
    if (bid._1 >= 0 && !bid._2.data.isEmpty) {
      if (blocks.known_then_load(bid)) {
        Some(blocks.get(bid).prosomoHeader)
      } else {
        None
      }
    } else {
      None
    }
  }

  /**
    * retrieve parent block
    * @param b
    * @return parent block if found, 0 otherwise
    */
  def getParentBlockHeader(b:BlockHeader): Option[BlockHeader] = {
    if (b._10 >= 0 && !b._1.data.isEmpty) {
      if (blocks.known_then_load((b._10,b._1))) {
        Some(blocks.get((b._10,b._1)).prosomoHeader)
      } else {
        None
      }
    } else {
      None
    }
  }

  /**
    * retrieve parent block id
    * @param bid
    * @return parent id if found, 0 otherwise
    */
  def getParentId(bid:SlotId): Option[SlotId] = {
    getBlockHeader(bid) match {
      case Some(b:BlockHeader) => Some((b._10,b._1))
      case _ => None
    }
  }

  def getNonce(id:SlotId):Option[Rho] = {
    getBlockHeader(id) match {
      case Some(header:BlockHeader) => Some(header._5)
      case _ => None
    }
  }

}
