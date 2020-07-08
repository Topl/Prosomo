package prosomo.history

import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.{Serializer, Tine}
import prosomo.primitives.{ByteStream, LDBStore, SimpleTypes}

import scala.util.Try

/**
  * AMS 2020:
  * Chain is stored to disk with each reorg so the node will have quick access to vrf nonces and slotIds
  * Local chain is restored from the last saved configuration upon restart
 */

class ChainStorage(dir:String) extends SimpleTypes {
  import prosomo.components.Serializer.DeserializeChain

  var chainStore:LDBStore = LDBStore(s"$dir/history/chain")

  def refresh():Unit = {
    chainStore.refresh()
  }

  def restore(cid:Hash,serializer: Serializer)(implicit blocks: BlockStorage):Tine = {
    chainStore.get(cid) match {
      case Some(bytes: ByteArrayWrapper) =>
        val byteStream: ByteStream = new ByteStream(bytes.data,DeserializeChain)
        Try{serializer.fromBytes(byteStream)}.toOption match {
          case Some(data:TineData@unchecked) => Tine(data)
          case _ =>
            new Tine
        }
      case None =>
        new Tine
    }
  }

  def store(chain:Tine, cid:Hash, serializer: Serializer):Unit  = {
    val cBytes = serializer.getBytes(chain)
    chainStore.update(Seq(),Seq(cid -> ByteArrayWrapper(cBytes)))
  }

}
