package prosomo.history

import prosomo.primitives.{ByteStream, Fch, LDBStore, SimpleTypes}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components
import prosomo.components.{Serializer, Wallet}

import scala.util.Try

/**
  * AMS 2020:
  * Wallet is saved to disk on reorg
  */

class WalletStorage(dir:String) extends SimpleTypes {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.storageFlag
  val fch = new Fch
  var walletStore:LDBStore = LDBStore(s"$dir/wallet")

  def refresh():Unit = {
    walletStore.refresh()
  }

  def restore(serializer: Serializer,pkw:ByteArrayWrapper):Wallet = if (storageFlag) {
    def newWallet:Wallet = {
      println("New wallet")
      val out = components.Wallet(pkw)
      store(out,serializer)
      out
    }
    walletStore.get(ByteArrayWrapper(fch.hash(pkw.data))) match {
      case Some(bytes: ByteArrayWrapper) => {
        val byteStream = new ByteStream(bytes.data,DeserializeWallet)
        Try{serializer.fromBytes(byteStream)}.toOption match {
          case Some(w:Wallet) if w.pkw == pkw =>
            println("Recovered wallet")
            w
          case _ => newWallet
        }
      }
      case _ => newWallet
    }
  } else {
    println("New wallet")
    components.Wallet(pkw)
  }

  def store(wallet:Wallet,serializer: Serializer):Unit  = if (storageFlag) {
    val wBytes = serializer.getBytes(wallet)
    val key = ByteArrayWrapper(fch.hash(wallet.pkw.data))
    walletStore.update(Seq(),Seq(key -> ByteArrayWrapper(wBytes)))
  }

}
