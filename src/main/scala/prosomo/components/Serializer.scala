package prosomo.components

import java.io.{ByteArrayOutputStream, ObjectOutputStream}

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.primitives._
import prosomo.wallet.Wallet

import scala.math.BigInt

class Serializer extends SimpleTypes {

  import Serializer._
  /*

   */

  //not to be used for serialization and deserialization, only use for router to calculate message length
  def getAnyBytes(input:Any):Array[Byte] = {
    input match {
      case block:Block => sBlock(block)
      case mac:Mac => sMac(mac)
      case transaction: Transaction => sTransaction(transaction)
      case blockHeader: BlockHeader => sBlockHeader(blockHeader)
      case ratio: Ratio => sRatio(ratio)
      case _ => serialize(input)
    }
  }

  //byte serializers for all types and classes
  def getBytes(bytes:Array[Byte]):Array[Byte] = bytes
  def getBytes(int:BigInt):Array[Byte] = sBigInt(int)
  def getBytes(int:Int):Array[Byte] = Ints.toByteArray(int)
  def getBytes(long: Long):Array[Byte] = Longs.toByteArray(long)
  def getBytes(bw:ByteArrayWrapper):Array[Byte] = bw.data
  def getBytes(string: String):Array[Byte] = sString(string)
  def getBytes(ratio:Ratio):Array[Byte] = sRatio(ratio)
  def getBytes(slotId: SlotId):Array[Byte] = Bytes.concat(getBytes(slotId._1),getBytes(slotId._2))
  def getBytes(cert:Cert):Array[Byte] = sCert(cert)
  def getBytes(kesSignature: KesSignature):Array[Byte] = sKesSignature(kesSignature)
  def getBytes(state: State):Array[Byte] = sState(state)
  def getBytes(transaction: Transaction):Array[Byte] = sTransaction(transaction)
  def getBytes(mac:Mac):Array[Byte] = sMac(mac)
  def getBytes(blockHeader: BlockHeader):Array[Byte] = sBlockHeader(blockHeader)
  def getBytes(gen:(Array[Byte], ByteArrayWrapper, BigInt,Mac)):Array[Byte] = sGen(gen)
  def getBytes(txs:TransactionSet):Array[Byte] = sTransactionSet(txs)
  def getBytes(idList:List[BlockId]):Array[Byte] = sIdList(idList)
  def getBytes(bool:Boolean):Array[Byte] = sBoolean(bool)
  def getBytes(chain:Tine):Array[Byte] = sChain(chain)
  def getBytes(wallet:Wallet):Array[Byte] = sWallet(wallet)
  def getBytes(malkinKey:MalkinKey):Array[Byte] = sMalkinKey(malkinKey)
  def getGenesisBytes(txs:GenesisSet):Array[Byte] = sGenesisSet(txs)

  def fromBytes(input:ByteStream): Any = {
    input.caseObject match {
      case DeserializeBlockHeader => dBlockHeader(input)
      case DeserializeMac => dMac(input)
      case DeserializeTransaction => dTransaction(input)
      case DeserializeGenesisSet => dGenesisSet(input)
      case DeserializeTransactionSet => dTransactionSet(input)
      case DeserializeIdList => dIdList(input)
      case DeserializeState => dState(input)
      case DeserializeChain => dChain(input)
      case DeserializeMalkinKey => dMalkinKey(input)
      case DeserializeWallet => dWallet(input)
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

  private def sBoolean(bool:Boolean):Array[Byte] = {
    if (bool) {
      Ints.toByteArray(1)
    } else {
      Ints.toByteArray(0)
    }
  }

  private def dBoolean(stream:ByteStream): Boolean = {
    val boolInt = stream.getInt
    assert(stream.empty && boolInt == 0 || boolInt == 1)
    boolInt match {
      case 0 => false
      case 1 => true
    }
  }

  private def sIdList(input:List[BlockId]):Array[Byte] ={
    def mapId(input:ByteArrayWrapper):Array[Byte] = if (input.data.isEmpty) {
      Array.fill[Byte](hash_length)(0.toByte)
    } else {input.data}
    Ints.toByteArray(input.length) ++ Bytes.concat(input.map(mapId):_*)
  }

  private def dIdList(stream:ByteStream):List[BlockId] = {
    val numEntries = stream.getInt
    var out:List[BlockId] = List()
    var i = 0
    val nullBytes = ByteArrayWrapper(Array.fill[Byte](hash_length)(0.toByte))
    while (i < numEntries) {
      val nextBytes = ByteArrayWrapper(stream.get(hash_length))
      if (nextBytes == nullBytes) {
        out = out ++ List(ByteArrayWrapper(Array()))
      } else {
        out = out ++ List(nextBytes)
      }
      i += 1
    }
    assert(out.length == numEntries)
    assert (stream.empty)
    out
  }

  private def sBigInt(int:BigInt):Array[Byte] = {
    val output = int.toByteArray
    Ints.toByteArray(output.length) ++ output
  }

  private def dBigInt(stream: ByteStream):BigInt = {
    val out = BigInt(stream.getAll)
    assert(stream.empty)
    out
  }

  private def sRatio(ratio: Ratio):Array[Byte] = {
    val out1 = sBigInt(ratio.numer)
    val out2 = sBigInt(ratio.denom)
    val output = Bytes.concat(out1,out2)
    Ints.toByteArray(output.length) ++ output
  }

  private def dRatio(stream: ByteStream):Ratio = {
    val out1len = stream.getInt
    val out1Bytes = new ByteStream(stream.get(out1len),Deserialize)
    val out1 = dBigInt(out1Bytes)
    val out2len = stream.getInt
    val out2Bytes = new ByteStream(stream.get(out2len),Deserialize)
    val out2 = dBigInt(out2Bytes)
    val out = new Ratio(
        out1,
        out2
    )
    assert(stream.empty)
    out
  }

  private def sString(string:String):Array[Byte] = {
    val output = string.getBytes
    Ints.toByteArray(output.length) ++ output
  }

  private def dString(stream: ByteStream):String = {
    val out = new String(stream.getAll)
    assert(stream.empty)
    out
  }

  private def sMac(mac: Mac):Array[Byte] = {
    val output = Bytes.concat(
      mac.dataHash.data,
      mac.sid.data,
      mac.signature,
      mac.publicKey
    )
    output
  }

  private def dMac(stream:ByteStream):Mac = {
    val out = new Mac(
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
      sBigInt(t.delta),
      t.sid.data,
      Ints.toByteArray(t.nonce),
      t.signature
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dTransaction(stream: ByteStream):Transaction = {
    val out1 = ByteArrayWrapper(stream.get(pkw_length))
    val out2 = ByteArrayWrapper(stream.get(pkw_length))
    val out3len = stream.getInt
    val out3Bytes = new ByteStream(stream.get(out3len),stream.caseObject)
    val out3 = dBigInt(out3Bytes)
    val out4 = ByteArrayWrapper(stream.get(sid_length))
    val out5 = stream.getInt
    val out6 = stream.get(sig_length)
    val out = Transaction(
      out1,
      out2,
      out3,
      out4,
      out5,
      out6
    )
    assert(stream.empty)
    out
  }

  private def sGen(gen:(Array[Byte], ByteArrayWrapper, BigInt,Mac)):Array[Byte] = {
    val output = Bytes.concat(
      gen._1,
      gen._2.data,
      sBigInt(gen._3),
      sMac(gen._4)
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dGen(stream: ByteStream):(Array[Byte], ByteArrayWrapper, BigInt,Mac) = {
    val out1 = stream.get(hash_length)
    val out2 = ByteArrayWrapper(stream.get(pkw_length))
    val out3len = stream.getInt
    val out3Bytes = new ByteStream(stream.get(out3len),stream.caseObject)
    val out3 = dBigInt(out3Bytes)
    val out4Bytes = new ByteStream(stream.get(mac_length),stream.caseObject)
    val out4 = dMac(out4Bytes)
    val out = (
      out1,
      out2,
      out3,
      out4
    )
    assert(stream.empty)
    out
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

  private def dBlockHeader(stream:ByteStream):BlockHeader = {
    val out1 = ByteArrayWrapper(stream.get(hash_length))
    val out2Bytes = new ByteStream(stream.get(mac_length),DeserializeMac)
    val out2 = dMac(out2Bytes)
    val out3 = stream.getInt
    val out4len = stream.getInt
    val out4Bytes = new ByteStream(stream.get(out4len),DeserializeBlockHeader)
    val out4 = dCert(out4Bytes)
    val out5 = stream.get(rho_length)
    val out6 = stream.get(pi_length)
    val out7len = stream.getInt
    val out7Bytes = new ByteStream(stream.get(out7len),DeserializeBlockHeader)
    val out7 = dKesSignature(out7Bytes)
    val out8 = stream.get(pk_length)
    val out9 = stream.getInt
    val out10 = stream.getInt
    val out = (
        out1,
        out2,
        out3,
        out4,
        out5,
        out6,
        out7,
        out8,
        out9,
        out10
    )
    assert(stream.empty)
    out
  }

  private def sCert(cert: Cert):Array[Byte] = {
    val output = Bytes.concat(
      getBytes(cert._1),
      getBytes(cert._2),
      getBytes(cert._3),
      getBytes(cert._4),
      getBytes(cert._5),
      getBytes(cert._6)
    )
    Ints.toByteArray(output.length) ++ output
  }

  private def dCert(stream:ByteStream):Cert = {
    val out1 = stream.get(pk_length)
    val out2 = stream.get(rho_length)
    val out3 = stream.get(pi_length)
    val out4 = stream.get(pk_length)
    val lenRatio = stream.getInt
    val ratioBytes = stream.get(lenRatio)
    val out5Bytes = new ByteStream(ratioBytes,DeserializeBlockHeader)
    val out5 = dRatio(out5Bytes)
    val lenString = stream.getInt
    val stringBytes = stream.get(lenString)
    val out6Bytes = new ByteStream(stringBytes,DeserializeBlockHeader)
    val out6 = dString(out6Bytes)
    val out = (
      out1,
      out2,
      out3,
      out4,
      out5,
      out6
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
    val out1len = stream.getInt
    val out1 = stream.get(out1len)
    val out2len = stream.getInt
    val out2 = stream.get(out2len)
    val out3len = stream.getInt
    val out3 = stream.get(out3len)
    val out = (
      out1,
      out2,
      out3
    )
    assert(stream.empty)
    out
  }

  private def sState(state: State):Array[Byte] = {
    def mapToBytes(in:(PublicKeyW,(BigInt,Boolean,Int))):Array[Byte] = {
      in._1.data ++ getBytes(in._2._1) ++ getBytes(in._2._2) ++ getBytes(in._2._3)
    }
    Ints.toByteArray(state.keySet.size) ++ Bytes.concat(state.toSeq.map(mapToBytes):_*)
  }

  private def dState(stream:ByteStream):State = {
    val numEntry = stream.getInt
    var out:State = Map()
    var i = 0
    while (i < numEntry) {
      val pkw = ByteArrayWrapper(stream.get(pkw_length))
      val biLen = stream.getInt
      val biBytes = new ByteStream(stream.get(biLen),stream.caseObject)
      val bi = dBigInt(biBytes)
      val boolBytes = new ByteStream(stream.get(4),stream.caseObject)
      val bool = dBoolean(boolBytes)
      val int = stream.getInt
      out += (pkw -> (bi,bool,int))
      i += 1
    }
    assert(out.keySet.size == numEntry)
    assert (stream.empty)
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
      val outLen = stream.getInt
      val outBytes = new ByteStream(stream.get(outLen),stream.caseObject)
      out = out ++ Seq(dTransaction(outBytes))
      i += 1
    }
    assert(out.length == numTx)
    assert (stream.empty)
    out
  }

  private def sGenesisSet(sequence: GenesisSet): Array[Byte] = {
    val output = Bytes.concat(sequence.map(getBytes):_*)
    Ints.toByteArray(sequence.length) ++ output
  }

  private def dGenesisSet(stream:ByteStream): GenesisSet = {
    val numTx = stream.getInt
    var out:GenesisSet = Seq()
    var i = 0
    while (i < numTx) {
      val outLen = stream.getInt
      val outBytes = new ByteStream(stream.get(outLen),stream.caseObject)
      out = out ++ Seq(dGen(outBytes))
      i += 1
    }
    assert(out.length == numTx)
    assert (stream.empty)
    out
  }

  private def sChain(chain:Tine):Array[Byte] = {
    Ints.toByteArray(chain.length) ++ Bytes.concat(chain.getData.map(getBytes):_*)
  }

  private def dChain(stream: ByteStream):Tine = {
    val numEntries = stream.getInt
    var out:Map[Slot,BlockId] = Map()
    var i = 0
    while (i < numEntries) {
      val slot = stream.getInt
      val id = ByteArrayWrapper(stream.get(hash_length))
      out += (slot->id)
      i += 1
    }
    assert(out.keySet.size == numEntries)
    assert(stream.empty)
    Tine(out)
  }

  private def sMalkinKey(key: MalkinKey):Array[Byte] = {
    Bytes.concat(
      sTree(key.L),
      sTree(key.Si),
      Ints.toByteArray(key.sig.length),
      key.sig,
      key.pki,
      key.rp
    )
  }

  private def dMalkinKey(stream:ByteStream):MalkinKey = {
    val out1len = stream.getInt
    val out1Bytes = new ByteStream(stream.get(out1len),stream.caseObject)
    val out1 = dTree(out1Bytes)
    val out2len = stream.getInt
    val out2Bytes = new ByteStream(stream.get(out2len),stream.caseObject)
    val out2 = dTree(out2Bytes)
    val out3len = stream.getInt
    val out3 = stream.get(out3len)
    val out4 = stream.get(pk_length)
    val out5 = stream.get(hash_length)
    assert(stream.empty)
    MalkinKey(out1,out2,out3,out4,out5)
  }

  private def sTree(tree:Tree[Array[Byte]]):Array[Byte] = {
    def treeToBytes(t:Tree[Array[Byte]]):Array[Byte] = {
      t match {
        case n:Node[Array[Byte]] => {
          n.l match {
            case Empty => {
              n.r match {
                case ll:Leaf[Array[Byte]] => {
                  Ints.toByteArray(2) ++ n.v ++ Ints.toByteArray(0) ++ ll.v
                }
                case nn:Node[Array[Byte]] => {
                  Ints.toByteArray(2) ++ n.v ++ treeToBytes(nn)
                }
              }
            }
            case ll:Leaf[Array[Byte]] => {
              Ints.toByteArray(1) ++ n.v ++ Ints.toByteArray(0) ++ ll.v
            }
            case nn:Node[Array[Byte]] => {
              Ints.toByteArray(1) ++ n.v ++ treeToBytes(nn)
            }
          }
        }
        case l:Leaf[Array[Byte]] => {
          Ints.toByteArray(0) ++ l.v
        }
      }
    }
    val output = treeToBytes(tree)
    Ints.toByteArray(output.length) ++ output
  }

  private def dTree(stream:ByteStream):Tree[Array[Byte]] = {
    def buildTree:Tree[Array[Byte]] = {
      stream.getInt match {
        case 0 => {
          val bytes:Array[Byte] = stream.get(sig_length)
          Leaf(bytes)
        }
        case 1 => {
          val bytes:Array[Byte] = stream.get(hash_length+sig_length)
          Node(bytes,buildTree,Empty)
        }
        case 2 => {
          val bytes:Array[Byte] = stream.get(hash_length+sig_length)
          Node(bytes,Empty,buildTree)
        }
      }
    }
    val out = buildTree
    assert(stream.empty)
    out
  }

  private def sBlock(block:Block):Array[Byte] = {
    val bodyBytes = block.body match {
      case txs:TransactionSet => getBytes(txs)
      case txs:GenesisSet => getGenesisBytes(txs)
    }
    Bytes.concat(block.id.data, getBytes(block.prosomoHeader), bodyBytes)
  }

  private def sTxMap(txs:Map[Sid,Transaction]):Array[Byte] = {
    def mapBytes(entry:(Sid,Transaction)):Array[Byte] = getBytes(entry._1) ++ getBytes(entry._2)
    val output = Bytes.concat(
      txs.toSeq.map(mapBytes(_)):_*
    )
    val total = Ints.toByteArray(txs.keySet.size) ++ output
    Ints.toByteArray(total.length) ++ total
  }

  private def dTxMap(stream: ByteStream):Map[Sid,Transaction] = {
    val numEntries = stream.getInt
    var out:Map[Sid,Transaction] = Map()
    var i = 0
    while (i < numEntries) {
      val sid:Sid = ByteArrayWrapper(stream.get(sid_length))
      val tx_len = stream.getInt
      val txBytes = new ByteStream(stream.get(tx_len),stream.caseObject)
      val tx:Transaction = dTransaction(txBytes)
      out += (sid->tx)
      i += 1
    }
    assert(out.keySet.size == numEntries)
    assert(stream.empty)
    out
  }

  private def sWallet(wallet: Wallet):Array[Byte] = {
    val stateBytes1 = getBytes(wallet.issueState)
    val stateBytes2 = getBytes(wallet.confirmedState)
    val output = Bytes.concat(
      wallet.pkw.data,
      getBytes(wallet.fee_r),
      sTxMap(wallet.pendingTxsOut),
      getBytes(wallet.availableBalance),
      getBytes(wallet.totalBalance),
      getBytes(wallet.txCounter),
      getBytes(wallet.confirmedTxCounter),
      getBytes(wallet.netStake),
      getBytes(wallet.netStake0),
      Ints.toByteArray(stateBytes1.length),
      stateBytes1,
      Ints.toByteArray(stateBytes2.length),
      stateBytes2
    )
    output
  }

  private def dWallet(stream: ByteStream):Wallet = {
    val out1:ByteArrayWrapper = ByteArrayWrapper(stream.get(pkw_length))
    val out2len = stream.getInt
    val out2Bytes = new ByteStream(stream.get(out2len),stream.caseObject)
    val out2:Ratio = dRatio(out2Bytes)
    val out = Wallet(out1,out2)
    val out3len = stream.getInt
    val b1 =new ByteStream(stream.get(out3len),stream.caseObject)
    out.pendingTxsOut = dTxMap(b1)
    val out4len = stream.getInt
    val b2 = new ByteStream(stream.get(out4len),stream.caseObject)
    out.availableBalance = dBigInt(b2)
    val out5len = stream.getInt
    val b3 = new ByteStream(stream.get(out5len),stream.caseObject)
    out.totalBalance = dBigInt(b3)
    out.txCounter = stream.getInt
    out.confirmedTxCounter = stream.getInt
    val out8len = stream.getInt
    val b4 = new ByteStream(stream.get(out8len),stream.caseObject)
    out.netStake = dBigInt(b4)
    val out9len = stream.getInt
    val b5 = new ByteStream(stream.get(out9len),stream.caseObject)
    out.netStake0 = dBigInt(b5)
    val out10len = stream.getInt
    val b6 = new ByteStream(stream.get(out10len),stream.caseObject)
    out.issueState = dState(b6)
    val out11len = stream.getInt
    val b7 = new ByteStream(stream.get(out11len),stream.caseObject)
    out.confirmedState = dState(b7)
    assert(stream.empty)
    out
  }

}

object Serializer {
  case object DeserializeBlock
  case object DeserializeBlockHeader
  case object DeserializeTransaction
  case object DeserializeMac
  case object DeserializeGenesisSet
  case object DeserializeTransactionSet
  case object DeserializeIdList
  case object DeserializeState
  case object DeserializeBoolean
  case object DeserializeChain
  case object DeserializeMalkinKey
  case object DeserializeWallet
  case object Deserialize
}