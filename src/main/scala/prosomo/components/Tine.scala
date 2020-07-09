package prosomo.components

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import prosomo.history.BlockStorage
import prosomo.primitives.SimpleTypes
import scorex.util.encode.Base58
import prosomo.primitives.Types._
import prosomo.primitives.Parameters.{one_third_epoch, slotWindow}

import scala.collection.mutable
import scala.util.Try

/**
  * AMS 2020:
  * Tine keeps track of a set of slotIds and VRF outputs for fast epoch nonce calculation
  * @param best an array that keeps track of the id with the highest block number in each third of an epoch
  * @param maxSlot option for tracking max slot of this tine
  * @param minSlot option for tracking min slot of this tine
  * @param blocks the database of all blocks
  */

case class Tine(var best:Map[BigInt,SlotId] = Map(),
                var maxSlot:Option[Slot] = None,
                var minSlot:Option[Slot] = None
               )(implicit blocks:BlockStorage) {

  val tineCache:LoadingCache[BigInt,mutable.SortedMap[Slot,(BlockId,Rho)]] = CacheBuilder.newBuilder()
    .maximumSize(12)
    .build[BigInt,mutable.SortedMap[Slot,(BlockId,Rho)]](new CacheLoader[BigInt,mutable.SortedMap[Slot,(BlockId,Rho)]] {
      def load(epoch3rd:BigInt):mutable.SortedMap[Slot,(BlockId,Rho)] = {
        if (best.keySet.contains(epoch3rd)) {
          val bestBlockId = best(epoch3rd)
          var out:mutable.SortedMap[Slot,(BlockId,Rho)] = mutable.SortedMap()
          var buildTine = true
          var testId = bestBlockId
          while (buildTine) {
            blocks.restoreHeader(testId) match {
              case Some(header) =>
                out += (header._3->(testId._2,header._5))
                if (header._10 < minSlot.get || BigInt(header._10/one_third_epoch) != epoch3rd) {
                  buildTine = false
                } else {
                  testId = (header._10,header._1)
                }
              case None => buildTine = false
            }
          }
          out
        } else {
          mutable.SortedMap()
        }
      }
    })

  def update(slotId:SlotId,nonce:Rho):Unit = {
    val cacheKey = BigInt(slotId._1/one_third_epoch)
    val newEntry:(BlockId,Rho) = (slotId._2,nonce)
    tineCache.get(cacheKey) += (slotId._1 -> newEntry)
    maxSlot = maxSlot match {
      case None => Some(slotId._1)
      case Some(slot) => Some(Seq(slot,slotId._1).max)
    }
    minSlot = minSlot match {
      case None => Some(slotId._1)
      case Some(slot) => Some(Seq(slot,slotId._1).min)
    }
    best.get(cacheKey) match {
      case None => best += (cacheKey -> slotId)
      case Some(bestId) =>
        if (slotId._1 > bestId._1) {
          best -= cacheKey
          best += (cacheKey -> slotId)
        }
    }
  }

  def get(slot:Slot):Option[SlotId] = {
    val cacheKey = BigInt(slot/one_third_epoch)
    tineCache.get(cacheKey).get(slot) match {
      case Some(data) => Some((slot,data._1))
      case None => None
    }
  }

  def getNonce(slot:Slot):Option[Rho] = {
    val cacheKey = BigInt(slot/one_third_epoch)
    tineCache.get(cacheKey).get(slot) match {
      case Some(data) => Some(data._2)
      case None => None
    }
  }

  def getNext(start:Slot,n:Int):Array[SlotId] = {
    var out:Array[SlotId] = Array()
    var index = start/one_third_epoch
    var done = false
    while (!done) {
      val cacheKey = BigInt(index)
      if (best.keySet.contains(cacheKey)) {
        val newCache = tineCache.get(cacheKey).filter(entry => entry._1 >= start)
        newCache.foreach(entry =>
          if (out.length < n) {
            out = out ++ Array(toSlotId(entry))
          } else {
            done = true
          }
        )
        index += 1
      } else {
        done = true
      }
    }
    out
  }

  def getLastActiveSlot(slot:Slot):Option[SlotId] = get(lastActiveSlot(slot).get)

  def lastActiveSlot(slot:Slot): Option[Slot] = {
    if (slot >= maxSlot.get) {
      maxSlot
    } else if (slot == minSlot.get) {
      minSlot
    } else if (slot < minSlot.get) {
      None
    } else {
      val cacheKey = BigInt(slot/one_third_epoch)
      Try{
        tineCache.get(cacheKey).keySet.filter(s => s <= slot).max
      }.toOption
    }
  }

  def head:SlotId = {
    get(maxSlot.get).get
  }

  def oldest:SlotId = {
    get(minSlot.get).get
  }

  def slice(min:Slot,max:Slot):Tine = {
    if (min < max) {
      var minOut:Option[Slot] = None
      var maxOut:Option[Slot] = None
      val out = new Tine
      if (!this.isEmpty) for (index <- min/one_third_epoch to max/one_third_epoch) {
        val cacheKey = BigInt(index)
        val newCache = tineCache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
        if (newCache.nonEmpty) {
          out.tineCache.put(cacheKey,newCache)
          val cacheMin = out.tineCache.get(cacheKey).keySet.min
          val cacheMax = out.tineCache.get(cacheKey).keySet.max
          minOut = minOut match {
            case None => Some(cacheMin)
            case _ => Some(Seq(cacheMin,minOut.get).min)
          }
          maxOut = maxOut match {
            case None => Some(cacheMax)
            case _ => Some(Seq(cacheMax,maxOut.get).max)
          }
          val bestId:SlotId = (cacheMax,out.tineCache.get(cacheKey)(cacheMax)._1)
          out.best += (cacheKey -> bestId)
        }
      }
      out.maxSlot = maxOut
      out.minSlot = minOut
      out
    } else if (min == max) {
      val out = new Tine
      this.get(max) match {
        case None =>
        case Some(id) =>
          this.getNonce(max) match {
            case None =>
            case Some(nonce) => out.update(id,nonce)
          }
      }
      out
    } else {
      new Tine
    }
  }

  def orderedNonceData(min:Slot,max:Slot):Array[Byte] = if (!this.isEmpty) {
    if (min < max) {
      var out:Array[Byte] = Array()
      for (index <- min/one_third_epoch to max/one_third_epoch) {
        val cacheKey = BigInt(index)
        val newCache = tineCache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
        newCache.foreach(entry => out ++= entry._2._2)
      }
      out
    } else if (min == max) {
      this.getNonce(max) match {
        case Some(nonce) => nonce
        case None => Array()
      }
    } else {
      Array()
    }
  } else {
    Array()
  }

  private def toSlotId(data:(Slot,(BlockId,Rho))):SlotId = (data._1,data._2._1)

  def ordered:Array[SlotId] = {
    var out:Array[SlotId] = Array()
    for (index <- best.keySet.toSeq.sorted) {
      out = out ++ tineCache.get(index).toArray.map(toSlotId)
    }
    out
  }

  def notSparsePast(prefix:Slot):Boolean = {
    var foundSlot = false
    for (index <- (prefix+1)/one_third_epoch to (prefix+slotWindow)/one_third_epoch) {
      if (!foundSlot) {
        val cacheKey = BigInt(index)
        tineCache.get(cacheKey).keySet.find(slot => slot > prefix && slot < prefix + slotWindow) match {
          case None =>
          case _ => foundSlot = true
        }
      }
    }
    foundSlot
  }

  def isEmpty:Boolean = {
    best.keySet.isEmpty
  }

  def numActive:Int = {
    maxSlot match {
      case None => 0
      case Some(max) =>
        minSlot match {
          case None => 0
          case Some(min) =>
            if (min == max) {
              1
            } else {
              var out = 0
              for (index <- best.keySet) {
                out += tineCache.get(index).keySet.size
              }
              out
            }
        }
    }
  }

  def print():Unit = {
    for (id<-this.ordered) {
      println("Slot:"+id._1.toString+" id:"+Base58.encode(id._2.data))
    }
  }

  def copy():Tine = {
    val out:Tine = new Tine
    out.minSlot = minSlot
    out.maxSlot = maxSlot
    out.best = best
    tineCache.asMap.keySet().forEach( key =>
      tineCache.getIfPresent(key) match {
        case value:mutable.SortedMap[Slot,(BlockId,Rho)] => out.tineCache.put(key,value)
        case _ =>
      }
    )
    out
  }

  def reorg(prefix:Slot,tine:Tine):Unit = {
    val prefixKey = BigInt(prefix/one_third_epoch)
    val newCache = tineCache.get(prefixKey).filter(data => data._1 <= prefix)
    tineCache.put(prefixKey,newCache)
    best = best.filter(data => data._1 < prefixKey)
    val newMax = newCache.keySet.max
    maxSlot = Some(newMax)
    val newBest:SlotId = (newMax,newCache(newMax)._1)
    best += (prefixKey -> newBest)
    for (id <- tine.ordered) {
      assert(id._1 > prefix)
      this.update(id,tine.getNonce(id._1).get)
    }
  }
}

object Tine extends SimpleTypes {

  def apply()(implicit blocks:BlockStorage):Tine = new Tine

  def apply(id:SlotId,nonce:Rho)(implicit blocks:BlockStorage):Tine = {
    val out = new Tine
    out.update(id,nonce)
    out
  }

  def apply(data:TineData)(implicit blocks:BlockStorage):Tine = {
    val out = new Tine
    out.best = data._1
    out.maxSlot = Some(data._2)
    out.minSlot = Some(data._3)
    out
  }

}
