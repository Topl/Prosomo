package prosomo.primitives

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock
import org.iq80.leveldb._
import io.iohk.iodb.ByteArrayWrapper

class LDBStore(file:File) {
  private val lock:ReentrantReadWriteLock = new ReentrantReadWriteLock()
  private val database:DB = {

    val nativeFactory = "org.fusesource.leveldbjni.JniDBFactory"
    val javaFactory = "org.iq80.leveldb.impl.Iq80DBFactory"

    def loadFactory(loader: ClassLoader, factoryName: String): Option[DBFactory] =
      try Some(loader.loadClass(factoryName).getConstructor().newInstance().asInstanceOf[DBFactory])
      catch {case e: Throwable => None}
    lazy val factory: DBFactory = {
      val loaders = List(ClassLoader.getSystemClassLoader, this.getClass.getClassLoader)
      val factories = List(nativeFactory, javaFactory)
      val pairs = loaders.view
        .zip(factories)
        .flatMap { case (loader, factoryName) =>
          loadFactory(loader, factoryName).map(factoryName -> _)
        }
      val (name, factory) = pairs.headOption.getOrElse(
        throw new RuntimeException(s"Could not load any of the factory classes: $nativeFactory, $javaFactory")
      )
      factory
    }
    val op = new Options()
    op.createIfMissing(true)
    factory.open(file, op)
  }

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

