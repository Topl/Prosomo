package prosomo.components

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import prosomo.primitives.Ratio
import com.google.common.primitives.{Bytes, Ints, Longs}
import io.iohk.iodb.ByteArrayWrapper

class Serializer extends SimpleTypes {

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

  def fromBytes[T:Manifest](input:Array[Byte]):Any = {
    val obj = manifest[T].runtimeClass.newInstance().asInstanceOf[T]
    obj match {
      case _ => obj
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
      box.sid.data,
      box.signature,
      box.publicKey,
      getAnyBytes(box.data)
    )
  }

  private def sTransaction(t: Transaction):Array[Byte] = {
    Bytes.concat(
      t.sender.data,
      t.receiver.data,
      t.delta.toByteArray,
      t.sid.data,
      Ints.toByteArray(t.nonce),
      t.signature
    )
  }

  private def sBlockHeader(bh:BlockHeader):Array[Byte] = {
    Bytes.concat(
      bh._1.data,
      getBytes(bh._2),
      getBytes(bh._3),
      getBytes(bh._4),
      bh._5,
      bh._6,
      getBytes(bh._7),
      bh._8,
      getBytes(bh._9),
      getBytes(bh._10)
    )
  }

  private def sCert(cert: Cert):Array[Byte] = {
    Bytes.concat(
      getBytes(cert._1),
      getBytes(cert._2),
      getBytes(cert._3),
      getBytes(cert._4),
      getBytes(cert._5),
      getBytes(cert._6)
    )
  }

  private def sKesSignature(kesSignature:KesSignature):Array[Byte] = {
    Bytes.concat(
      kesSignature._1,
      kesSignature._2,
      kesSignature._3
    )
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

  private def sBlock(block:Block):Array[Byte] = {
    val bodyBytes = block.body match {
      case sequence:Seq[Transaction] => Bytes.concat(sequence.map(getBytes):_*)
      case sequence:Seq[Box] => Bytes.concat(sequence.map(getBytes):_*)
    }
    val headerBytes = getBytes(block.prosomoHeader)
    headerBytes ++ bodyBytes
  }


}
