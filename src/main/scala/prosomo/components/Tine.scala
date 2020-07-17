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
  import Tine._

  /**
    * Initializing tineCache is resource intensive so the database is handled with Either Left Right logic,
    * The database is started Left with a single SortedMap for all slots,
    * Once the Left database expands the update method will turn it Right and load the tineCache
    */

   private lazy val tineCache:LoadingCache[BigInt,TineCache] = CacheBuilder.newBuilder()
    .maximumSize(12)
    .build[BigInt,TineCache](
      new CacheLoader[BigInt,TineCache] {
        def load(epoch3rd:BigInt):TineCache = {
          if (best.keySet.contains(epoch3rd)) {
            val bestBlockId = best(epoch3rd)
            var out:TineCache = emptyTineCache
            var buildTine = true
            var testId = bestBlockId
            while (buildTine) {
              blocks.restoreHeader(testId) match {
                case Some(header) =>
                  out =  append(out,(header._3,testId._2,header._5))
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
            emptyTineCache
          }
        }
      }
    )

  private var tineDB:Either[
    TineCache,
    LoadingCache[BigInt,TineCache]
  ] = Left(emptyTineCache)

  def loadCache():Unit = {
    tineDB match {
      case Left(cache) if cache.nonEmpty =>
        tineDB = Right(tineCache)
        minSlot = None
        maxSlot = None
        best = mutable.SortedMap()
        cache.foreach(entry => this.update((entry._1,entry._2),entry._3))
      case _ =>
        tineDB = Right(tineCache)
    }
  }

  def update(slotId:SlotId,nonce:Rho):Unit = Try{
    val newEntry = (slotId._1,slotId._2,nonce)
    tineDB match {
      case Left(cache) =>
        if (cache.isEmpty) {
          maxSlot = Some(slotId._1)
          minSlot = Some(slotId._1)
          tineDB = Left(append(cache,newEntry))
        } else {
          maxSlot match {
            case Some(slot) if slotId._1 > slot =>
              assert(toSlotId(cache.last) == blocks.get(slotId).get.parentSlotId)
              tineDB = Left(append(cache,newEntry))
              maxSlot = Some(slotId._1)
            case _ =>
          }
          minSlot match {
            case Some(slot) if slotId._1 < slot =>
              assert(slotId == blocks.get(toSlotId(cache.head)).get.parentSlotId)
              tineDB = Left(prepend(cache,newEntry))
              minSlot = Some(slotId._1)
            case _ =>
          }
        }
      case Right(loadingCache) =>
        val cacheKey = BigInt(slotId._1/one_third_epoch)
        val cache:TineCache = loadingCache.get(cacheKey)
        if (cache.isEmpty) {
          maxSlot match {
            case Some(slot) if slotId._1 > slot => maxSlot = Some(slotId._1)
            case None => maxSlot = Some(slotId._1)
            case _ =>
          }
          minSlot match {
            case Some(slot) if slotId._1 < slot => minSlot = Some(slotId._1)
            case None => minSlot = Some(slotId._1)
            case _ =>
          }
          loadingCache.invalidate(cacheKey)
          loadingCache.put(cacheKey,append(cache,newEntry))
          best += (cacheKey -> slotId)
        } else {
          maxSlot match {
            case Some(slot) if slotId._1 > slot =>
              loadingCache.invalidate(cacheKey)
              loadingCache.put(cacheKey,append(cache,newEntry))
              maxSlot = Some(slotId._1)
            case _ =>
          }
          minSlot match {
            case Some(slot) if slotId._1 < slot =>
              loadingCache.invalidate(cacheKey)
              loadingCache.put(cacheKey,prepend(cache,newEntry))
              minSlot = Some(slotId._1)
            case _ =>
          }
          best.get(cacheKey) match {
            case Some(bestId) if slotId._1 >= bestId._1 =>
              best -= cacheKey
              best += (cacheKey -> slotId)
            case Some(bestId) if slotId._1 < bestId._1 =>
            case None =>
              best += (cacheKey -> slotId)
          }
        }
    }
  } match {
    case Failure(e) => e.printStackTrace()
    case _ =>
  }

  def get(slot:Slot):Option[SlotId] = {
    tineDB match {
      case Left(cache) =>
        cache.find(entry => entry._1 == slot) match {
          case Some(data) => Some((slot,data._2))
          case None => None
        }
      case Right(cache) =>
        val cacheKey = BigInt(slot/one_third_epoch)
        cache.get(cacheKey).find(entry => entry._1 == slot) match {
          case Some(data) => Some((slot,data._2))
          case None => None
        }
    }
  }

  def getNonce(slot:Slot):Option[Rho] = {
    tineDB match {
      case Left(cache) =>
        cache.find(entry => entry._1 == slot)  match {
          case Some(data) => Some(data._3)
          case None => None
        }
      case Right(cache) =>
        val cacheKey = BigInt(slot/one_third_epoch)
        cache.get(cacheKey).find(entry => entry._1 == slot)  match {
          case Some(data) => Some(data._3)
          case None => None
        }
    }
  }

  def getNext(start:Slot,n:Int):Array[SlotId] = {
    tineDB match {
      case Left(cache) =>
        var out:Array[SlotId] = Array()
        val filtered = cache.filter(entry => entry._1 >= start)
        filtered.foreach(entry =>
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
            val filtered = cache.get(cacheKey).filter(entry => entry._1 >= start)
            filtered.foreach(entry =>
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

  def lastActiveSlot(slot:Slot): Option[Slot] = Try{
    if (slot >= maxSlot.get) {
      maxSlot.get
    } else if (slot == minSlot.get) {
      minSlot.get
    } else if (slot < minSlot.get) {
      None.get
    } else {
      tineDB match {
        case Left(cache) =>
          Try{
            cache.filter(data => data._1 <= slot).last._1
          }.toOption.get
        case Right(cache) =>
          var index = slot/one_third_epoch
          var done = false
          var out:Option[Slot] = None
          while(!done) {
            val cacheKey = BigInt(index)
            cache.get(cacheKey).filter(data => data._1 <= slot) match {
              case filtered if filtered.isEmpty =>
                if (index>0) {
                  index -= 1
                } else {
                  done = true
                }
              case filtered if filtered.nonEmpty =>
                out = Some(filtered.last._1)
                done = true
            }
          }
          out.get
      }
    }
  } match {
    case Success(slot) => Some(slot)
    case Failure(e) =>
      e.printStackTrace()
      None
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
          out.minSlot = Try{newCache.head._1}.toOption
          out.maxSlot = Try{newCache.last._1}.toOption
          out
        case Right(cache) => if (max - min > slotWindow) {
          var minOut:Option[Slot] = None
          var maxOut:Option[Slot] = None
          val out = new Tine
          for (index <- min/one_third_epoch to max/one_third_epoch) {
            val cacheKey = BigInt(index)
            val newCache = cache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
            if (newCache.nonEmpty) {
              val cacheMin = newCache.head._1
              val cacheMax = newCache.last._1
              minOut = minOut match {
                case None => Some(cacheMin)
                case _ => Some(Array(cacheMin,minOut.get).min)
              }
              maxOut = maxOut match {
                case None => Some(cacheMax)
                case _ => Some(Array(cacheMax,maxOut.get).max)
              }
              val bestId:SlotId = toSlotId(newCache.last)
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
          var outCache:TineCache = emptyTineCache
          if (!this.isEmpty) for (index <- min/one_third_epoch to max/one_third_epoch) {
            val cacheKey = BigInt(index)
            val newCache = cache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
            if (newCache.nonEmpty) {
              val cacheMin = newCache.head._1
              val cacheMax = newCache.last._1
              minOut = minOut match {
                case None => Some(cacheMin)
                case _ => Some(Array(cacheMin,minOut.get).min)
              }
              maxOut = maxOut match {
                case None => Some(cacheMax)
                case _ => Some(Array(cacheMax,maxOut.get).max)
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
              val filtered = cache.filter(data => data._1 < t.minSlot.get && data._1 >= min)
              Bytes.concat(
                Bytes.concat(filtered.map(entry => entry._3):_*),
                t.orderedNonceData(t.minSlot.get,max,None)
              )
            case Right(cache) =>
              var out:Array[Byte] = Array()
              for (index <- min/one_third_epoch to (t.minSlot.get-1)/one_third_epoch) {
                val cacheKey = BigInt(index)
                val newCache = cache.get(cacheKey).filter(data => data._1 < t.minSlot.get && data._1 >= min)
                out = Bytes.concat(out, Bytes.concat(newCache.map(entry => entry._3):_*))
              }
              Bytes.concat(out, t.orderedNonceData(t.minSlot.get,max,tine))
          }
        case _ =>
          tineDB match {
            case Left(cache) =>
              val newCache = cache.filter(data => data._1 <= max && data._1 >= min)
              Bytes.concat(newCache.map(entry => entry._3):_*)
            case Right(cache) =>
              var out:Array[Byte] = Array()
              for (index <- min/one_third_epoch to max/one_third_epoch) {
                val cacheKey = BigInt(index)
                val newCache = cache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
                out = Bytes.concat(out, Bytes.concat(newCache.map(entry => entry._3):_*))
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

  private def toSlotId(data:(Slot,BlockId,Rho)):SlotId = (data._1,data._2)

  def ordered:Array[SlotId] = {
    var out:Array[SlotId] = Array()
    tineDB match {
      case Left(cache) =>
        cache.map(toSlotId)
      case Right(cache) =>
        for (index <- best.keySet) {
          out = out ++ cache.get(index).map(toSlotId)
        }
        out
    }
  }

  def notSparsePast(prefix:Slot):Boolean = {
    var foundSlot = false
    tineDB match {
      case Left(cache) =>
        cache.find(data => data._1 > prefix && data._1 < prefix + slotWindow) match {
          case None =>
          case _ => foundSlot = true
        }
      case Right(cache) =>
        for (index <- (prefix+1)/one_third_epoch to (prefix+slotWindow)/one_third_epoch) {
          if (!foundSlot) {
            val cacheKey = BigInt(index)
            cache.get(cacheKey).find(data => data._1 > prefix && data._1 < prefix + slotWindow) match {
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
        cache.isEmpty
      case Right(_) =>
        best.isEmpty
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
                case Left(cache) => cache.length
                case Right(cache) =>
                  var out = 0
                  for (index <- best.keySet) {
                    out += cache.get(index).length
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
            case value:TineCache => out.tineCache.put(key,value)
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
          assert(minSlot.get == cache.head._1)
          assert(maxSlot.get == cache.last._1)
          var id:SlotId = toSlotId(cache.last)
          var block:Block = blocks.get(id).get
          var nonce:Rho = block.nonce
          assert(nonce sameElements cache.last._3)
          for (entry <- cache.reverse.tail) {
            val pid = block.parentSlotId
            if (toSlotId(entry) != pid) {
              val thisId = toSlotId(entry)
              println(pid._1.toString+":"+Base58.encode(pid._2.data))
              println(id._1.toString+":"+Base58.encode(id._2.data))
              println(thisId._1.toString+":"+Base58.encode(thisId._2.data))
            }
            assert(toSlotId(entry) == pid)
            id = pid
            block = blocks.get(id).get
            nonce = block.nonce
            assert(nonce sameElements entry._3)
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
          var cachePid:Option[SlotId] = None
          assert(id._1 == maxSlot.get)
          for (value <- best.toArray.reverse) {
            val cache = loaderCache.get(value._1)
            id = toSlotId(cache.last)
            assert(cachePid match {
              case None => true
              case Some(cid) => cid == id
            })
            assert(id == best(value._1))
            var block:Block = blocks.get(id).get
            var nonce:Rho = block.nonce
            assert(nonce sameElements cache.last._3)
            for (entry <- cache.reverse.tail) {
              val pid = block.parentSlotId
              if (toSlotId(entry) != pid) {
                val thisId = toSlotId(entry)
                println(pid._1.toString+":"+Base58.encode(pid._2.data))
                println(id._1.toString+":"+Base58.encode(id._2.data))
                println(thisId._1.toString+":"+Base58.encode(thisId._2.data))
              }
              assert(toSlotId(entry) == pid)
              id = pid
              block = blocks.get(id).get
              nonce = block.nonce
              assert(nonce sameElements entry._3)
            }
            if (id._1 > 0) cachePid = Some(blocks.get(id).get.parentSlotId)
          }
        }
    }
  } match {
    case Success(_) => true
    case Failure(exception) =>
      exception.printStackTrace()
      false
  }

  def reorg(prefix:Slot,tine:Tine):Unit = Try{
    if (maxSlot.get == prefix && prefix < tine.minSlot.get) {
      for (id <- tine.ordered) {
        this.update(id,tine.getNonce(id._1).get)
      }
    } else {
      tineDB match {
        case Left(cache) =>
          val newCache = cache.filter(data => data._1 <= prefix)
          tineDB = Left(newCache)
          val newMax = newCache.last._1
          maxSlot = Some(newMax)
          for (id <- tine.ordered) {
            this.update(id,tine.getNonce(id._1).get)
          }
        case Right(cache) =>
          val prefixKey = BigInt(prefix/one_third_epoch)
          val newCache = cache.get(prefixKey).filter(data => data._1 <= prefix)
          best.keySet.filter(key => key >= prefixKey).foreach(key => cache.invalidate(key))
          best = best.filter(data => data._1 < prefixKey)
          cache.put(prefixKey,newCache)
          val newMax = newCache.last._1
          maxSlot = Some(newMax)
          val newBest:SlotId = toSlotId(newCache.last)
          best += (prefixKey -> newBest)
          for (id <- tine.ordered) {
            this.update(id,tine.getNonce(id._1).get)
          }
      }
    }

  } match {
    case Failure(exception) => exception.printStackTrace()
    case _ =>
  }
}

object Tine extends SimpleTypes {

  type SlotData = (Slot,BlockId,Rho)
  type TineCache = Array[SlotData]
  def emptyTineCache:TineCache = Array.empty
  def append(tineCache: TineCache,input:SlotData):Array[SlotData] =
    tineCache ++ Array(input)
  def prepend(tineCache:TineCache,input:SlotData):Array[SlotData] =
    Array(input) ++ tineCache


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
