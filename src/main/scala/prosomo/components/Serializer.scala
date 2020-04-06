package prosomo.components

import prosomo.primitives._

class Serializer extends SimpleTypes with SerializationMethods

object Serializer {
  case object DeserializeBlock
  case object DeserializeGenesisBlock
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
  case object DeserializeDiffuse
  case object DeserializeHello
  case object DeserializeRequestBlock
  case object DeserializeRequestTine
  case object DeserializeReturnBlocks
  case object DeserializeSendBlock
  case object DeserializeSendTx
  case object DeserializeHoldersFromRemote
  case object Deserialize
}