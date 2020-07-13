package prosomo.components

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.google.common.primitives.Bytes
import prosomo.history.BlockStorage
import prosomo.primitives.SimpleTypes
import scorex.util.encode.Base58
import prosomo.primitives.Types._
import prosomo.primitives.Parameters.{one_third_epoch, slotWindow}

import scala.collection.mutable
import scala.util.{Try,Success,Failure}

/**
  * AMS 2020:
  * Tine keeps track of a set of slotIds and VRF outputs for fast epoch nonce calculation
  * @param best an array that keeps track of the id with the highest block number in each third of an epoch
  * @param maxSlot option for tracking max slot of this tine
  * @param minSlot option for tracking min slot of this tine
  * @param blocks the database of all blocks
  */

case class Tine(var best:mutable.SortedMap[BigInt,SlotId] = mutable.SortedMap(),
                var maxSlot:Option[Slot] = None,
                var minSlot:Option[Slot] = None
               )(implicit blocks:BlockStorage) {

  /**
    * Initializing tineCache is resource intensive so the database is handled with Either Left Right logic,
    * The database is started Left with a single SortedMap for all slots,
    * Once the Left database expands the update method will turn it Right and load the tineCache
    */

   private lazy val tineCache:LoadingCache[BigInt,mutable.SortedMap[Slot,(BlockId,Rho)]] = CacheBuilder.newBuilder()
    .maximumSize(12)
    .build[BigInt,mutable.SortedMap[Slot,(BlockId,Rho)]](
      new CacheLoader[BigInt,mutable.SortedMap[Slot,(BlockId,Rho)]] {
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
      }
    )

  private var tineDB:Either[
    mutable.SortedMap[Slot,(BlockId,Rho)],
    LoadingCache[BigInt,mutable.SortedMap[Slot,(BlockId,Rho)]]
  ] = Left(mutable.SortedMap())

  def loadCache():Unit = {
    tineDB match {
      case Left(cache) if cache.keySet.nonEmpty =>
        tineDB = Right(tineCache)
        minSlot = None
        maxSlot = None
        best = mutable.SortedMap()
        cache.foreach(entry => update((entry._1,entry._2._1),entry._2._2))
      case _ =>
        tineDB = Right(tineCache)
    }
  }

  def update(slotId:SlotId,nonce:Rho):Unit = {
    val newEntry:(BlockId,Rho) = (slotId._2,nonce)
    tineDB match {
      case Left(cache) =>
        cache += (slotId._1 -> newEntry)
        maxSlot = maxSlot match {
          case None => Some(slotId._1)
          case Some(slot) => Some(Seq(slot,slotId._1).max)
        }
        minSlot = minSlot match {
          case None => Some(slotId._1)
          case Some(slot) => Some(Seq(slot,slotId._1).min)
        }
      case Right(cache) =>
        val cacheKey = BigInt(slotId._1/one_third_epoch)
        cache.get(cacheKey) += (slotId._1 -> newEntry)
        best.get(cacheKey) match {
          case None => best += (cacheKey -> slotId)
          case Some(bestId) =>
            if (slotId._1 > bestId._1) {
              best -= cacheKey
              best += (cacheKey -> slotId)
            }
        }
        maxSlot = maxSlot match {
          case None => Some(slotId._1)
          case Some(slot) => Some(Seq(slot,slotId._1).max)
        }
        minSlot = minSlot match {
          case None => Some(slotId._1)
          case Some(slot) => Some(Seq(slot,slotId._1).min)
        }
    }
  }

  def get(slot:Slot):Option[SlotId] = {
    tineDB match {
      case Left(cache) =>
        cache.get(slot) match {
          case Some(data) => Some((slot,data._1))
          case None => None
        }
      case Right(cache) =>
        val cacheKey = BigInt(slot/one_third_epoch)
        cache.get(cacheKey).get(slot) match {
          case Some(data) => Some((slot,data._1))
          case None => None
        }
    }
  }

  def getNonce(slot:Slot):Option[Rho] = {
    tineDB match {
      case Left(cache) =>
        cache.get(slot) match {
          case Some(data) => Some(data._2)
          case None => None
        }
      case Right(cache) =>
        val cacheKey = BigInt(slot/one_third_epoch)
        cache.get(cacheKey).get(slot) match {
          case Some(data) => Some(data._2)
          case None => None
        }
    }
  }

  def getNext(start:Slot,n:Int):Array[SlotId] = {
    tineDB match {
      case Left(cache) =>
        var out:Array[SlotId] = Array()
        val newCache = cache.filter(entry => entry._1 >= start)
        newCache.foreach(entry =>
          if (out.length < n) {
            out = out ++ Array(toSlotId(entry))
          }
        )
        out
      case Right(cache) =>
        var out:Array[SlotId] = Array()
        var index = start/one_third_epoch
        var done = false
        while (!done) {
          val cacheKey = BigInt(index)
          if (best.keySet.contains(cacheKey)) {
            val newCache = cache.get(cacheKey).filter(entry => entry._1 >= start)
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
      tineDB match {
        case Left(cache) =>
          Try{
            cache.keySet.filter(s => s <= slot).max
          }.toOption
        case Right(cache) =>
          val cacheKey = BigInt(slot/one_third_epoch)
          Try{
            cache.get(cacheKey).keySet.filter(s => s <= slot).max
          }.toOption
      }
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
      tineDB match {
        case Left(cache) =>
          val out = new Tine
          val newCache = cache.filter(data => data._1 <= max && data._1 >= min)
          out.tineDB = Left(newCache)
          out.minSlot = Try{newCache.keySet.min}.toOption
          out.maxSlot = Try{newCache.keySet.max}.toOption
          out
        case Right(cache) => if (max - min > slotWindow) {
          var minOut:Option[Slot] = None
          var maxOut:Option[Slot] = None
          val out = new Tine
          if (!this.isEmpty) for (index <- min/one_third_epoch to max/one_third_epoch) {
            val cacheKey = BigInt(index)
            val newCache = cache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
            if (newCache.nonEmpty) {
              val cacheMin = newCache.keySet.min
              val cacheMax = newCache.keySet.max
              minOut = minOut match {
                case None => Some(cacheMin)
                case _ => Some(Seq(cacheMin,minOut.get).min)
              }
              maxOut = maxOut match {
                case None => Some(cacheMax)
                case _ => Some(Seq(cacheMax,maxOut.get).max)
              }
              val bestId:SlotId = (cacheMax,newCache(cacheMax)._1)
              out.best += (cacheKey -> bestId)
              out.loadCache()
              out.tineCache.put(cacheKey,newCache)
            }
          }
          out.minSlot = minOut
          out.maxSlot = maxOut
          out
        } else {
          var minOut:Option[Slot] = None
          var maxOut:Option[Slot] = None
          val out = new Tine
          val outCache:mutable.SortedMap[Slot,(BlockId,Rho)] = mutable.SortedMap()
          if (!this.isEmpty) for (index <- min/one_third_epoch to max/one_third_epoch) {
            val cacheKey = BigInt(index)
            val newCache = cache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
            if (newCache.nonEmpty) {
              val cacheMin = newCache.keySet.min
              val cacheMax = newCache.keySet.max
              minOut = minOut match {
                case None => Some(cacheMin)
                case _ => Some(Seq(cacheMin,minOut.get).min)
              }
              maxOut = maxOut match {
                case None => Some(cacheMax)
                case _ => Some(Seq(cacheMax,maxOut.get).max)
              }
              outCache ++= newCache
            }
          }
          out.tineDB = Left(outCache)
          out.minSlot = minOut
          out.maxSlot = maxOut
          out
        }
      }
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

  def orderedNonceData(min:Slot,max:Slot,tine:Option[Tine]):Array[Byte] = {
    if (min < max) {
      tine match {
        case Some(t) if t.minSlot.get <= min =>
          t.orderedNonceData(min,max,None)
        case Some(t) if t.minSlot.get <= max =>
          tineDB match {
            case Left(cache) =>
              val newCache = cache.filter(data => data._1 < t.minSlot.get && data._1 >= min)
              Bytes.concat(
                Bytes.concat(newCache.toArray.map(entry => entry._2._2):_*),
                t.orderedNonceData(t.minSlot.get,max,None)
              )
            case Right(cache) =>
              var out:Array[Byte] = Array()
              for (index <- min/one_third_epoch to (t.minSlot.get-1)/one_third_epoch) {
                val cacheKey = BigInt(index)
                val newCache = cache.get(cacheKey).filter(data => data._1 < t.minSlot.get && data._1 >= min)
                out = Bytes.concat(out, Bytes.concat(newCache.toArray.map(entry => entry._2._2):_*))
              }
              Bytes.concat(out, t.orderedNonceData(t.minSlot.get,max,tine))
          }
        case _ =>
          tineDB match {
            case Left(cache) =>
              val newCache = cache.filter(data => data._1 <= max && data._1 >= min)
              Bytes.concat(newCache.toArray.map(entry => entry._2._2):_*)
            case Right(cache) =>
              var out:Array[Byte] = Array()
              for (index <- min/one_third_epoch to max/one_third_epoch) {
                val cacheKey = BigInt(index)
                val newCache = cache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
                out = Bytes.concat(out, Bytes.concat(newCache.toArray.map(entry => entry._2._2):_*))
              }
              out
          }
      }
    } else if (min == max) {
      tine match {
        case Some(t) if t.minSlot.get <= max =>
          t.getNonce(max) match {
            case Some(nonce) => nonce
            case None => Array()
          }
        case _ =>
          this.getNonce(max) match {
            case Some(nonce) => nonce
            case None => Array()
          }
      }
    } else {
      Array()
    }
  }

  private def toSlotId(data:(Slot,(BlockId,Rho))):SlotId = (data._1,data._2._1)

  def ordered:Array[SlotId] = {
    var out:Array[SlotId] = Array()
    tineDB match {
      case Left(cache) =>
        cache.toArray.map(toSlotId)
      case Right(cache) =>
        for (index <- best.keySet.toSeq.sorted) {
          out = out ++ cache.get(index).toArray.map(toSlotId)
        }
        out
    }
  }

  def notSparsePast(prefix:Slot):Boolean = {
    var foundSlot = false
    tineDB match {
      case Left(cache) =>
        cache.keySet.find(slot => slot > prefix && slot < prefix + slotWindow) match {
          case None =>
          case _ => foundSlot = true
        }
      case Right(cache) =>
        for (index <- (prefix+1)/one_third_epoch to (prefix+slotWindow)/one_third_epoch) {
          if (!foundSlot) {
            val cacheKey = BigInt(index)
            cache.get(cacheKey).keySet.find(slot => slot > prefix && slot < prefix + slotWindow) match {
              case None =>
              case _ => foundSlot = true
            }
          }
        }
    }
    foundSlot
  }

  def isEmpty:Boolean = {
    tineDB match {
      case Left(cache) =>
        cache.keySet.isEmpty
      case Right(_) =>
        best.keySet.isEmpty
    }
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
              tineDB match {
                case Left(cache) => cache.keySet.size
                case Right(cache) =>
                  var out = 0
                  for (index <- best.keySet) {
                    out += cache.get(index).keySet.size
                  }
                  out
              }
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
    tineDB match {
      case Left(cache) =>
        out.tineDB = Left(cache)
      case Right(cache) =>
        out.loadCache()
        cache.asMap.keySet().forEach( key =>
          cache.getIfPresent(key) match {
            case value:mutable.SortedMap[Slot,(BlockId,Rho)] => out.tineCache.put(key,value)
            case _ =>
          }
        )
    }
    out
  }

  def copy(tine:Tine):Unit = {
    tineDB match {
      case Left(_) =>
        this.minSlot = tine.minSlot
        this.maxSlot = tine.maxSlot
      case Right(_) =>
        this.minSlot = tine.minSlot
        this.maxSlot = tine.maxSlot
        this.best = tine.best
    }
  }

  def verify:Boolean = Try{
    tineDB match {
      case Left(cache) =>
        assert(best.isEmpty)
        if (cache.nonEmpty) {
          assert(maxSlot.get == cache.keySet.max)
          assert(minSlot.get == cache.keySet.min)
          var id:SlotId = toSlotId(cache.keySet.max,cache(cache.keySet.max))
          for (entry <- cache.toArray.reverse.tail) {
            val block = blocks.get(id).get
            assert(block.nonce sameElements cache(id._1)._2)
            if (id._1 > 0) {
              val pid = block.parentSlotId
              assert(toSlotId(entry) == pid)
              id = pid
            }
          }
        } else {
          assert(maxSlot == None)
          assert(minSlot == None)
        }
      case Right(loaderCache) =>
        if (best.isEmpty) {
          assert(maxSlot == None)
          assert(minSlot == None)
        } else {
          var id:SlotId = best(best.keySet.max)
          var cpid:Option[SlotId] = None
          assert(id._1 == maxSlot.get)
          for (value <- best.toArray.reverse) {
            val cache = loaderCache.get(value._1)
            id = toSlotId(cache.keySet.max,cache(cache.keySet.max))
            assert(cpid match {
              case None => true
              case Some(cid) => cid == id
            })
            assert(id == best(value._1))
            for (entry <- cache.toArray.reverse.tail) {
              val block = blocks.get(id).get
              assert(block.nonce sameElements cache(id._1)._2)
              if (id._1 > 0) {
                val pid = block.parentSlotId
                if (toSlotId(entry) != pid) {
                  val thisId = toSlotId(entry)
                  println(pid._1.toString+":"+Base58.encode(pid._2.data))
                  println(id._1.toString+":"+Base58.encode(id._2.data))
                  println(thisId._1.toString+":"+Base58.encode(thisId._2.data))
                }
                assert(toSlotId(entry) == pid)
                id = pid
              }
            }
            if (id._1 > 0) cpid = Some(blocks.get(id).get.parentSlotId)
          }
        }
    }
  } match {
    case Success(_) => true
    case Failure(exception) =>
      exception.printStackTrace()
      false
  }

  def reorg(prefix:Slot,tine:Tine):Unit = {
    assert(prefix >= minSlot.get)
    tineDB match {
      case Left(cache) =>
        val newCache = cache.filter(data => data._1 <= prefix)
        tineDB = Left(newCache)
        for (id <- tine.ordered) {
          update(id,tine.getNonce(id._1).get)
        }
      case Right(cache) =>
        if (maxSlot.get < tine.head._1 && tine.numActive == 1) {
          for (id <- tine.ordered) {
            update(id,tine.getNonce(id._1).get)
          }
        } else {
          val prefixKey = BigInt(prefix/one_third_epoch)
          val newCache = cache.get(prefixKey).filter(data => data._1 <= prefix)
          best.keySet.filter(key => key >= prefixKey).foreach(key => cache.invalidate(key))
          best = best.filter(data => data._1 < prefixKey)
          cache.put(prefixKey,newCache)
          val newMax = newCache.keySet.max
          maxSlot = Some(newMax)
          val newBest:SlotId = (newMax,newCache(newMax)._1)
          best += (prefixKey -> newBest)
          for (id <- tine.ordered) {
            update(id,tine.getNonce(id._1).get)
          }
        }
    }
  }
}

object Tine extends SimpleTypes {

  def apply()(implicit blocks:BlockStorage):Tine = new Tine

  //for new heads in tinePool
  def apply(id:SlotId,nonce:Rho)(implicit blocks:BlockStorage):Tine = {
    val out = new Tine
    out.update(id,nonce)
    out
  }

  //for loading localChain data from db
  def apply(data:TineData)(implicit blocks:BlockStorage):Tine = {
    val out = new Tine
    out.loadCache()
    out.best = data._1
    out.minSlot = Some(data._2)
    out.maxSlot = Some(data._3)
    out
  }

}
