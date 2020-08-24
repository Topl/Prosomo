package prosomo.ledger

import prosomo.primitives.Types.{PublicKeyW, Sid, Signature}

import scala.math.BigInt

/**
 * AMS 2020:
 * Simple account transactions, used for research and experiments
 */

case class Transaction(sender: PublicKeyW, receiver: PublicKeyW, delta: BigInt, sid: Sid, nonce: Int, signature: Signature)
