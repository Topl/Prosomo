package prosomo.history

import prosomo.primitives.{ByteStream, Fch, LDBStore, Ratio, SimpleTypes}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.components.Serializer
import prosomo.wallet.Wallet


class WalletStorage(dir:String) extends SimpleTypes {
  import prosomo.components.Serializer._
  import prosomo.primitives.Parameters.storageFlag
  val fch = new Fch
  var walletStore:LDBStore = LDBStore(s"$dir/wallet")

  def refresh:Unit = {
    walletStore.refresh()
  }

  def restore(serializer: Serializer,pkw:ByteArrayWrapper,fee_r:Ratio):Wallet = if (storageFlag) {
    def newWallet:Wallet = {
      println("New wallet")
      val out = Wallet(pkw,fee_r)
      store(out,serializer)
      out
    }
    walletStore.get(ByteArrayWrapper(fch.hash(pkw.data))) match {
      case Some(bytes: ByteArrayWrapper) => {
        val byteStream = new ByteStream(bytes.data,DeserializeWallet)
        serializer.fromBytes(byteStream) match {
          case w:Wallet if w.pkw == pkw => {
            println("Recovered wallet")
            w
          }
          case _ => newWallet
        }
      }
      case _ => newWallet
    }
  } else {
    println("New wallet")
    Wallet(pkw,fee_r)
  }

  def uuid: String = java.util.UUID.randomUUID.toString

  def store(wallet:Wallet,serializer: Serializer):Unit  = if (storageFlag) {
    val wBytes = serializer.getBytes(wallet)
    val key = ByteArrayWrapper(fch.hash(wallet.pkw.data))
    walletStore.update(Seq(),Seq(key -> ByteArrayWrapper(wBytes)))
  }

}
