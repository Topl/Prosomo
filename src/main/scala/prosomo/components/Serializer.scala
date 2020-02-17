package prosomo.components

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import prosomo.primitives.Ratio
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.iohk.iodb.ByteArrayWrapper

import scala.math.BigInt

class Serializer extends SimpleTypes {

  import Serializer._
  /*

   */

  def getAnyBytes(input:Any):Array[Byte] = {
    input match {
      case block:Block => sBlock(block)
      case box:Box => sBox(box)
      case transaction: Transaction => sTransaction(transaction)
      case blockHeader: BlockHeader => sBlockHeader(blockHeader)
      case ratio: Ratio => sRatio(ratio)
      case _ => serialize(input)
    }
  }

  def getBytes(bytes:Array[Byte]):Array[Byte] = bytes
  def getBytes(int:Int):Array[Byte] = Ints.toByteArray(int)
  def getBytes(long: Long):Array[Byte] = Longs.toByteArray(long)
  def getBytes(bw:ByteArrayWrapper):Array[Byte] = bw.data
  def getBytes(string: String):Array[Byte] = string.getBytes
  //def getBytes(hash:Hash):Array[Byte] = hash.data
  //def getBytes(eta:Eta):Array[Byte] = eta
  //def getBytes(signature: Signature):Array[Byte] = signature
  //def getBytes(slot:Slot):Array[Byte] = Ints.toByteArray(slot)
  //def getBytes(blockNumber: BlockNumber):Array[Byte] = Ints.toByteArray(blockNumber)
  //def getBytes(rho:Rho):Array[Byte] = rho
  //def getBytes(publicKey: PublicKey):Array[Byte] = publicKey
  //def getBytes(privateKey: PrivateKey):Array[Byte] = privateKey
  //def getBytes(sid:Sid):Array[Byte] = sid.data
  //def getBytes(publicKeyW: PublicKeyW):Array[Byte] = publicKeyW.data
  //def getBytes(publicKeys: PublicKeys):Array[Byte] = Bytes.concat(publicKeys._1,publicKeys._2,publicKeys._3)
  //def getBytes(pi:Pi):Array[Byte] = pi
  //def getBytes(blockId: BlockId):Array[Byte] = blockId.data
  def getBytes(ratio:Ratio):Array[Byte] = sRatio(ratio)
  def getBytes(slotId: SlotId):Array[Byte] = Bytes.concat(getBytes(slotId._1),getBytes(slotId._2))
  def getBytes(cert:Cert):Array[Byte] = sCert(cert)
  def getBytes(kesSignature: KesSignature):Array[Byte] = sKesSignature(kesSignature)
//  def getBytes(chainRequest: ChainRequest):Array[Byte] = Bytes.concat(getBytes(chainRequest._1),getBytes(chainRequest._2),getBytes(chainRequest._3))
//  def getBytes(blockRequest: BlockRequest):Array[Byte] = Bytes.concat(getBytes(blockRequest._1),getBytes(blockRequest._2))
  def getBytes(state: State):Array[Byte] = sState(state)
  def getBytes(transaction: Transaction):Array[Byte] = sTransaction(transaction)
  def getBytes(box:Box):Array[Byte] = sBox(box)
  def getBytes(blockHeader: BlockHeader):Array[Byte] = sBlockHeader(blockHeader)
  def getBytes(gen:(Array[Byte], ByteArrayWrapper, BigInt,Box)):Array[Byte] = sGen(gen)
  def getBytes(txs:TransactionSet):Array[Byte] = sTransactionSet(txs)
  def getGenesisBytes(txs:GenesisSet):Array[Byte] = sGenesisSet(txs)

  def fromBytes[T:Manifest](input:Array[Byte]):Any = {
    val obj = manifest[T].runtimeClass.newInstance().asInstanceOf[T]
    obj match {
      case _ => obj
    }
  }

  def fromBytes(input:ByteStream): Any = {
    input.co match {
      case DeserializeBlockHeader => dBlockHeader(input)
      case DeserializeBox => dBox(input)
      case DeserializeTransaction => dTransaction(input)
    }
  }

  /**
    * Byte serialization
    * @param value any object to be serialized
    * @return byte array
    */
  private def serialize(value: Any): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close()
    stream.toByteArray
  }

  /**
    * Deserialize a byte array that was serialized with serialize
    * @param bytes byte array processed with serialize
    * @return original object
    */
  private def deserialize(bytes: Array[Byte]): Any = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close()
    value
  }

  private def sBox(box: Box):Array[Byte] = {
    Bytes.concat(
      box.dataHash.data,
      box.sid.data,
      box.signature,
      box.publicKey
    )
  }

  private def dBox(stream:ByteStream):Box = {
    val out = new Box(
      ByteArrayWrapper(stream.get(hash_length)),
      ByteArrayWrapper(stream.get(hash_length)),
      stream.get(sig_length),
      stream.get(pk_length)
    )
    assert(stream.empty)
    out
  }

  private def sTransaction(t: Transaction):Array[Byte] = {
    val output = Bytes.concat(
      t.sender.data,
      t.receiver.data,
      Ints.toByteArray(t.delta.toByteArray.length),
      t.delta.toByteArray,
      t.sid.data,
      Ints.toByteArray(t.nonce),
      t.signature
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dTransaction(stream: ByteStream):Transaction = {
    val out = new Transaction(
      ByteArrayWrapper(stream.get(pkw_length)),
      ByteArrayWrapper(stream.get(pkw_length)),
      BigInt(stream.get(stream.getInt)),
      ByteArrayWrapper(stream.get(sid_length)),
      stream.getInt,
      stream.get(sig_length)
    )
    assert(stream.empty)
    out
  }

  private def sGen(gen:(Array[Byte], ByteArrayWrapper, BigInt,Box)):Array[Byte] = {
    val output = Bytes.concat(
      gen._1,
      gen._2.data,
      Ints.toByteArray(gen._3.toByteArray.length),
      gen._3.toByteArray,
      sBox(gen._4)
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dGen(stream: ByteStream):(Array[Byte], ByteArrayWrapper, BigInt,Box) = {
    val out = (
      stream.get(hash_length),
      ByteArrayWrapper(stream.get(pkw_length)),
      BigInt(stream.get(stream.getInt)),
      dBox(ByteStream(stream.get(box_length),DeserializeBox))
    )
    assert(stream.empty)
    out
  }

  private def sBlockHeader(bh:BlockHeader):Array[Byte] = {
    val certBytes = getBytes(bh._4)
    val kesSigBytes = getBytes(bh._7)
    Bytes.concat(
      bh._1.data,
      getBytes(bh._2),
      getBytes(bh._3),
      Ints.toByteArray(certBytes.length),
      certBytes,
      bh._5,
      bh._6,
      Ints.toByteArray(kesSigBytes.length),
      kesSigBytes,
      bh._8,
      getBytes(bh._9),
      getBytes(bh._10)
    )
  }

  private def dBlockHeader(stream:ByteStream):BlockHeader = {
    val out = (
      ByteArrayWrapper(stream.get(hash_length)),
      dBox(ByteStream(stream.get(box_length),DeserializeBox)),
      stream.getInt,
      dCert(ByteStream(stream.get(stream.getInt),DeserializeBlockHeader)),
      stream.get(rho_length),
      stream.get(pi_length),
      dKesSignature(ByteStream(stream.get(stream.getInt),DeserializeBlockHeader)),
      stream.get(pk_length),
      stream.getInt,
      stream.getInt
    )
    assert(stream.empty)
    out
  }

  private def sCert(cert: Cert):Array[Byte] = {
    val len5 = Ints.toByteArray(getBytes(cert._5).length)
    val len6 = Ints.toByteArray(getBytes(cert._6).length)
    val output = Bytes.concat(
      getBytes(cert._1),
      getBytes(cert._2),
      getBytes(cert._3),
      getBytes(cert._4),
      len5,
      getBytes(cert._5),
      len6,
      getBytes(cert._6)
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dCert(stream:ByteStream):Cert = {
    val out = (
      stream.get(pk_length),
      stream.get(rho_length),
      stream.get(pi_length),
      stream.get(pk_length),
      dRatio(ByteStream(stream.get(stream.getInt),DeserializeBlockHeader)),
      new String(stream.get(stream.getInt))
    )
    assert(stream.empty)
    out
  }

  private def sKesSignature(kesSignature:KesSignature):Array[Byte] = {
    val output = Bytes.concat(
      Ints.toByteArray(kesSignature._1.length),
      kesSignature._1,
      Ints.toByteArray(kesSignature._2.length),
      kesSignature._2,
      Ints.toByteArray(kesSignature._3.length),
      kesSignature._3
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dKesSignature(stream: ByteStream): KesSignature = {
    val out = (
      stream.get(stream.getInt),
      stream.get(stream.getInt),
      stream.get(stream.getInt)
    )
    assert(stream.empty)
    out
  }

  private def sState(state: State):Array[Byte] = {
    Bytes.concat(state.toSeq.map(getAnyBytes):_*)
  }

  private def sRatio(ratio: Ratio):Array[Byte] = {
    val data1 = ratio.numer.toByteArray
    val data2 = ratio.denom.toByteArray
    Bytes.concat(
      getBytes(data1.length),
      data1,
      getBytes(data2.length),
      data2
    )
  }

  private def dRatio(stream: ByteStream):Ratio = {
    val out = new Ratio(
      BigInt(stream.get(stream.getInt)),
      BigInt(stream.get(stream.getInt))
    )
    assert(stream.empty)
    out
  }

  private def sTransactionSet(sequence:TransactionSet):Array[Byte] = {
    val bodyBytes = Bytes.concat(sequence.map(getBytes):_*)
    Ints.toByteArray(sequence.length) ++ bodyBytes
  }

  private def dTransactionSet(stream: ByteStream):TransactionSet = {
    val numTx = stream.getInt
    var out:TransactionSet = Seq()
    var i = 0
    while (i < numTx) {
      out = out ++ Seq(dTransaction(ByteStream(stream.get(stream.getInt),DeserializeTransaction)))
      i += 1
    }
    assert(out.length == numTx)
    assert (stream.empty)
    out
  }

  private def sGenesisSet(sequence: GenesisSet): Array[Byte] = {
    val bodyBytes = Bytes.concat(sequence.map(getBytes):_*)
    Ints.toByteArray(sequence.length) ++ bodyBytes
  }

  private def dGenesisSet(stream:ByteStream): GenesisSet = {
    val numTx = stream.getInt
    var out:GenesisSet = Seq()
    var i = 0
    while (i < numTx) {
      out = out ++ Seq(dGen(ByteStream(stream.get(stream.getInt),DeserializeTransaction)))
      i += 1
    }
    assert(out.length == numTx)
    assert (stream.empty)
    out
  }

  private def sBlock(block:Block):Array[Byte] = {
    val bodyBytes = block.body match {
      case txs:TransactionSet => getBytes(txs)
      case txs:GenesisSet => getGenesisBytes(txs)
    }
    Bytes.concat(block.id.data, getBytes(block.prosomoHeader), bodyBytes)
  }

}

object Serializer {
  case object DeserializeBlock
  case object DeserializeBlockHeader
  case object DeserializeTransaction
  case object DeserializeBox
}