package prosomo.primitives

import prosomo.primitives.Types.Hash

/**
  * AMS 2020:
  * Hash based message authentication for verifying all messages from network
  * verification occurs with a time stamp
  */

case class Mac(hash:Hash,time:Long)