package prosomo.components

import prosomo.primitives.Types._
import scala.math.BigInt

/*
 Simple account transactions
 */

case class Transaction(sender:PublicKeyW,receiver:PublicKeyW,delta:BigInt,sid:Sid,nonce:Int,signature: Signature)