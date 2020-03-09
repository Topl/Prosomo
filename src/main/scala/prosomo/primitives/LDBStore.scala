package prosomo.primitives

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock
import org.iq80.leveldb._
import org.fusesource.leveldbjni.JniDBFactory
import org.iq80.leveldb.impl.Iq80DBFactory._

import io.iohk.iodb.ByteArrayWrapper

class LDBStore(dir:String) {

  private val lock:ReentrantReadWriteLock = new ReentrantReadWriteLock()

  val iFile = new File(dir)
  iFile.mkdirs()
  //jniFactoryName = "org.fusesource.leveldbjni.JniDBFactory"
  //javaFactoryName = "org.iq80.leveldb.impl.Iq80DBFactory"
  val op = new Options()
  op.createIfMissing(true)
  op.paranoidChecks(true)
  op.blockSize(1 * 1024 * 1024)
  op.cacheSize(4 * 1024 * 1024)
  op.maxOpenFiles(10)
  var database:DB = factory.open(iFile, op)
//  val newThread = new Thread() {
//    override def run(): Unit = {
//      database.close()
//    }
//  }
//  Runtime.getRuntime.addShutdownHook(newThread)

  //database.suspendCompactions()

  def get(key:ByteArrayWrapper):Option[ByteArrayWrapper] = {
    lock.readLock().lock()
    val result = try {
      Option(database.get(key.data))
    } finally {
      lock.readLock().unlock()
    }
    result match {
      case Some(bytes:Array[Byte]) => Some(ByteArrayWrapper(bytes))
      case _ => None
    }
  }

  def known(key:ByteArrayWrapper):Boolean = {
    get(key) match {
      case Some(bytes:ByteArrayWrapper) => true
      case None => false
    }
  }

  def update(remove:Seq[ByteArrayWrapper],insert:Seq[(ByteArrayWrapper,ByteArrayWrapper)]):Unit = {
    val dbBatch = database.createWriteBatch()
    try {
      for (entry <- remove) {
        dbBatch.delete(entry.data)
      }
      for (entry <- insert) {
        dbBatch.put(entry._1.data,entry._2.data)
      }
      database.write(dbBatch)
    } finally {
      dbBatch.close()
    }
  }

  def close(): Unit = {
    lock.writeLock().lock()
    try {
      database.close()
    } finally {
      lock.writeLock().unlock()
    }
  }

}

