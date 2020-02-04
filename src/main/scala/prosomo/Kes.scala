package prosomo

import bifrost.crypto.hash.FastCryptographicHash
import org.bouncycastle.math.ec.rfc8032.Ed25519

import scala.math.BigInt


/**
  * Implementation of the MMM construction:
  * Malkin, T., Micciancio, D. and Miner, S. (2002) ‘Efficient generic
  * forward-secure signatures with an unbounded number of time
  * periods’, Advances in Cryptology Eurocrypt ’02, LNCS 2332,
  * Springer, pp.400–417.
  *
  * Provides forward secure signatures that cannot be reforged with a leaked private key that has been updated
  * Number of time steps is determined by logl argument upon key generation, practically unbounded for logl = 7
  * Sum compostion is based on underlying Ed25519 signing routine provided by Bouncy Castle
  */

class Kes {

  val seedBytes = 32
  val pkBytes = Ed25519.PUBLIC_KEY_SIZE
  val skBytes = Ed25519.SECRET_KEY_SIZE
  val sigBytes = Ed25519.SIGNATURE_SIZE
  val hashBytes = 32
  val KeyLength = hashBytes
  val logl = 7

  type MalkinKey = (Tree[Array[Byte]],Tree[Array[Byte]],Array[Byte],Array[Byte],Array[Byte])
  type MalkinSignature = (Array[Byte],Array[Byte],Array[Byte])

  /**
    * Exponent base two of the argument
    * @param n integer
    * @return 2 to the n
    */
  def exp(n: Int): Int = {
    scala.math.pow(2,n).toInt
  }

  /**
    * Pseudorandom number generator used for seed doubling
    * input must be non-recoverable from from k and outputs
    * cannot be used to determine one from the other
    * @param k
    * @return
    */

  def PRNG(k: Array[Byte]): (Array[Byte],Array[Byte]) = {
    val r1 = FastCryptographicHash(k)
    val r2 = FastCryptographicHash(r1++k)
    (r1,r2)
  }

  /**
    * generates a keypair for Ed25519 signing and returns it in a single byte array
    * @param seed input entropy for keypair generation
    * @return byte array sk||pk
    */
  def sKeypairFast(seed: Array[Byte]): Array[Byte] = {
    val sk = FastCryptographicHash(seed)
    var pk = Array.fill(32){0x00.toByte}
    Ed25519.generatePublicKey(sk,0,pk,0)
    sk++pk
  }

  /**
    * Returns only the public key for a given seed
    * @param seed input entropy for keypair generation
    * @return byte array pk
    */
  def sPublic(seed: Array[Byte]): Array[Byte] = {
    val sk = FastCryptographicHash(seed)
    var pk = Array.fill(32){0x00.toByte}
    Ed25519.generatePublicKey(sk,0,pk,0)
    pk
  }

  /**
    * Returns only the private key for a given seed
    * @param seed input entropy for keypair generation
    * @return byte array sk
    */
  def sPrivate(seed: Array[Byte]): Array[Byte] = {
    FastCryptographicHash(seed)
  }

  /**
    * Signing routine for Ed25519
    * @param m message to be signed
    * @param sk Ed25519 secret key to be signed
    * @return Ed25519 signature
    */
  def sSign(m: Array[Byte], sk: Array[Byte]): Array[Byte] = {
    var sig: Array[Byte] = Array.fill(sigBytes){0x00.toByte}
    Ed25519.sign(sk,0,m,0,m.length,sig,0)
    sig
  }

  /**
    * Verify routine for Ed25519
    * @param m message for given signature
    * @param sig signature to be verified
    * @param pk public key corresponding to signature
    * @return true if valid signature, false if otherwise
    */
  def sVerify(m: Array[Byte], sig: Array[Byte], pk: Array[Byte]): Boolean = {
    Ed25519.verify(sig,0,pk,0,m,0,m.length)
  }

  /**
    * Gets the public key in the sum composition
    * @param t binary tree for which the key is to be calculated
    * @return binary array public key
    */
  def sumGetPublicKey(t: Tree[Array[Byte]]): Array[Byte] = {
    t match {
      case n: Node[Array[Byte]] => {
        val pk0 = n.v.slice(seedBytes, seedBytes + pkBytes)
        val pk1 = n.v.slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)
        FastCryptographicHash(pk0 ++ pk1)
      }
      case l: Leaf[Array[Byte]] => {
        FastCryptographicHash(FastCryptographicHash(l.v.slice(seedBytes, seedBytes + pkBytes))++FastCryptographicHash(l.v.slice(seedBytes, seedBytes + pkBytes)))
      }
      case _ => Array()
    }
  }

  /**
    * Generates keys in the sum composition, recursive functions construct the tree in steps and the output is
    * the leftmost branch
    * @param seed input entropy for binary tree and keypair generation
    * @param i height of tree
    * @return binary tree at time step 0
    */
  def sumGenerateKey(seed: Array[Byte],i:Int):Tree[Array[Byte]] = {

    // generate the binary tree with the pseudorandom number generator
    def sumKeyGenMerkle(seed: Array[Byte],i:Int): Tree[Array[Byte]] = {
      if (i==0){
        Leaf(seed)
      } else {
        val r = PRNG(seed)
        Node(r._2,sumKeyGenMerkle(r._1,i-1),sumKeyGenMerkle(r._2,i-1))
      }
    }

    // generates the Ed25519 keypairs on each leaf
    def populateLeaf(t: Tree[Array[Byte]]): Tree[Array[Byte]] = {
      t match {
        case n: Node[Array[Byte]] => {
          Node(n.v,populateLeaf(n.l),populateLeaf(n.r))
        }
        case l: Leaf[Array[Byte]] => {
          Leaf(l.v++sKeypairFast(l.v))
        }
        case _ => {
          Empty
        }
      }
    }

    // generates the Merkle tree of the public keys and stores the hash values on each node
    def merklePublicKeys(t: Tree[Array[Byte]]): Tree[Array[Byte]] = {
      def loop(t: Tree[Array[Byte]]): Tree[Array[Byte]] = {
        t match {
          case n: Node[Array[Byte]] => {
            var sk0: Array[Byte] = Array()
            var pk0: Array[Byte] = Array()
            var pk00: Array[Byte] = Array()
            var pk01: Array[Byte] = Array()
            var sk1: Array[Byte] = Array()
            var pk1: Array[Byte] = Array()
            var pk10: Array[Byte] = Array()
            var pk11: Array[Byte] = Array()
            var pk: Array[Byte] = Array()
            var r0: Array[Byte] = Array()
            var r1: Array[Byte] = Array()
            var leftVal: Array[Byte] = Array()
            var rightVal: Array[Byte] = Array()
            var leafLevel = false
            val left = loop(n.l) match {
              case nn: Node[Array[Byte]] => {
                leftVal = nn.v
                nn
              }
              case ll: Leaf[Array[Byte]] => {
                leafLevel = true
                leftVal = ll.v
                ll
              }
            }
            val right = loop(n.r) match {
              case nn: Node[Array[Byte]] => {
                rightVal = nn.v
                nn
              }
              case ll: Leaf[Array[Byte]] => {
                leafLevel = true
                rightVal = ll.v
                ll
              }
            }
            if (leafLevel) {
              r0 = leftVal.slice(0, seedBytes)
              sk0 = leftVal.slice(seedBytes, seedBytes + skBytes)
              pk0 = leftVal.slice(seedBytes + skBytes, seedBytes + skBytes + pkBytes)
              r1 = rightVal.slice(0, seedBytes)
              sk1 = rightVal.slice(seedBytes, seedBytes + skBytes)
              pk1 = rightVal.slice(seedBytes + skBytes, seedBytes + skBytes + pkBytes)
              assert(n.v.deep == r1.deep)
              Node(n.v ++ FastCryptographicHash(pk0) ++ FastCryptographicHash(pk1), Leaf(sk0 ++ pk0), Leaf(sk1 ++ pk1))
            } else {
              pk00 = leftVal.slice(seedBytes, seedBytes + pkBytes)
              pk01 = leftVal.slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)
              pk10 = rightVal.slice(seedBytes, seedBytes + pkBytes)
              pk11 = rightVal.slice(seedBytes + pkBytes, seedBytes + 2 * pkBytes)
              pk0 = FastCryptographicHash(pk00 ++ pk01)
              pk1 = FastCryptographicHash(pk10 ++ pk11)
              Node(n.v ++ pk0 ++ pk1, left, right)
            }
          }
          case l: Leaf[Array[Byte]] => {
            l
          }
          case _ => {
            Empty
          }
        }
      }
      t match {
        case n: Node[Array[Byte]] => {
          loop(n)
        }
        case l: Leaf[Array[Byte]] => {
          Leaf(l.v.drop(seedBytes))
        }
        case _ => {
          Empty
        }
      }
    }

    //removes all but the leftmost branch leaving the leftmost leaf
    def trimTree(t: Tree[Array[Byte]]): Tree[Array[Byte]] = {
      t match {
        case n: Node[Array[Byte]] => {
          Node(n.v,trimTree(n.l),Empty)
        }
        case l: Leaf[Array[Byte]] => {
          l
        }
        case _ => {
          Empty
        }
      }
    }

    //executes the above functions in order
    trimTree(merklePublicKeys(populateLeaf(sumKeyGenMerkle(seed,i))))
  }

  /**
    * Verify a public key with a binary tree
    * @param t binary tree that contains Merkle tree hash values
    * @param pk root of the Merkle tree
    * @return true if pk is the root of the Merkle tree, false if otherwise
    */
  def sumVerifyKeyPair(t: Tree[Array[Byte]], pk:Array[Byte]): Boolean = {
    //loops through the tree to verify Merkle witness path
    def loop(t: Tree[Array[Byte]]): Boolean = {
      t match {
        case n: Node[Array[Byte]] =>{
          var pk0:Array[Byte] = Array()
          var pk00:Array[Byte] = Array()
          var pk01:Array[Byte] = Array()
          var pk1:Array[Byte] = Array()
          var pk10:Array[Byte] = Array()
          var pk11:Array[Byte] = Array()
          val left = n.l match {
            case nn: Node[Array[Byte]] => {
              pk00 = nn.v.slice(seedBytes,seedBytes+pkBytes)
              pk01 = nn.v.slice(seedBytes+pkBytes,seedBytes+2*pkBytes)
              pk0 = FastCryptographicHash(pk00++pk01)
              loop(nn) && (pk0.deep == n.v.slice(seedBytes,seedBytes+pkBytes).deep)
            }
            case ll: Leaf[Array[Byte]] => {
              FastCryptographicHash(ll.v.slice(skBytes,skBytes+pkBytes)).deep == n.v.slice(seedBytes,seedBytes+pkBytes).deep
            }
            case _ => true
          }
          val right = n.r match {
            case nn: Node[Array[Byte]] => {
              pk10 = nn.v.slice(seedBytes,seedBytes+pkBytes)
              pk11 = nn.v.slice(seedBytes+pkBytes,seedBytes+2*pkBytes)
              pk1 = FastCryptographicHash(pk10++pk11)
              loop(nn) && (pk1.deep == n.v.slice(seedBytes+pkBytes,seedBytes+2*pkBytes).deep)
            }
            case ll: Leaf[Array[Byte]] => {
              FastCryptographicHash(ll.v.slice(skBytes,skBytes+pkBytes)).deep == n.v.slice(seedBytes+pkBytes,seedBytes+2*pkBytes).deep
            }
            case _ => true
          }
          left && right
        }
        case l: Leaf[Array[Byte]] => FastCryptographicHash(FastCryptographicHash(l.v.slice(skBytes,skBytes+pkBytes))++FastCryptographicHash(l.v.slice(skBytes,skBytes+pkBytes))).deep == pk.deep
        case _ => false
      }
    }
    (pk.deep == sumGetPublicKey(t).deep) && loop(t)
  }

  /**
    * Updates the key in the sum composition
    * @param key binary tree to be updated
    * @param t time step key is to be updated to
    * @return updated key to be written to key
    */
  def sumUpdate(key: Tree[Array[Byte]],t:Int): Tree[Array[Byte]] = {
    //checks if the sub tree is right most
    def isRightBranch(t: Tree[Array[Byte]]): Boolean = {
      t match {
        case n: Node[Array[Byte]] =>{
          val left = n.l match {
            case n: Node[Array[Byte]] => false
            case l: Leaf[Array[Byte]] => false
            case _ => true
          }
          val right = n.r match {
            case n: Node[Array[Byte]] => isRightBranch(n)
            case l: Leaf[Array[Byte]] => true
            case _ => false
          }
          left && right
        }
        case l: Leaf[Array[Byte]] => false
        case _ => false
      }
    }

    //main loop that steps the tree to the next time step
    def loop(t: Tree[Array[Byte]]): Tree[Array[Byte]] = {
      t match {
        case n: Node[Array[Byte]] => {
          var leftIsEmpty = false
          var leftIsLeaf = false
          var leftIsNode = false
          var leftVal: Array[Byte] = Array()
          var rightIsEmpty = false
          var rightIsLeaf = false
          var rightIsNode = false
          var rightVal: Array[Byte] = Array()

          val left = n.l match {
            case n: Node[Array[Byte]] => leftIsNode = true;leftVal=n.v;n
            case l: Leaf[Array[Byte]] => leftIsLeaf = true;leftVal=l.v;l
            case _ => leftIsEmpty = true; n.l
          }
          val right = n.r match {
            case n: Node[Array[Byte]] => rightIsNode=true;rightVal=n.v;n
            case l: Leaf[Array[Byte]] => rightIsLeaf=true;rightVal=l.v;l
            case _ => rightIsEmpty = true; n.r
          }
          val cutBranch = isRightBranch(left)
          if (rightIsEmpty && leftIsLeaf) {
            //println("right is empty and left is leaf")
            val keyPair = sKeypairFast(n.v.slice(0,seedBytes))
            assert(FastCryptographicHash(keyPair.slice(skBytes,skBytes+pkBytes)).deep == n.v.slice(seedBytes+pkBytes,seedBytes+2*pkBytes).deep)
            Node(n.v,Empty,Leaf(keyPair))
          } else if (cutBranch) {
            //println("cut branch")
            Node(n.v,Empty,sumGenerateKey(n.v.slice(0,seedBytes),n.height-1))
          } else if (leftIsNode && rightIsEmpty) {
            //println("left is node and right is empty")
            Node(n.v,loop(left),Empty)
          } else if (leftIsEmpty && rightIsNode) {
            //println("left is empty and right is node")
            Node(n.v, Empty, loop(right))
          } else if (leftIsEmpty && rightIsLeaf) {
            //println("Error: cut branch failed, left is empty and right is leaf")
            n
          } else if (leftIsEmpty && rightIsEmpty) {
            //println("Error: left and right is empty")
            n
          } else {
            //println("Error: did nothing")
            n
          }
        }
        case l: Leaf[Array[Byte]] => l
        case _ => t
      }
    }
    val keyH = key.height
    val T = exp(key.height)
    val keyTime = sumGetKeyTimeStep(key)
    //steps key through time steps one at a time until key step == t
    if (t<T && keyTime < t){
      var tempKey = key
      for(i <- keyTime+1 to t) {
        tempKey = loop(tempKey)
      }
      tempKey
    } else {
      println("Time step error, key not updated")
      println("T: "+T.toString+", key t:"+keyTime.toString+", t:"+t.toString)
      key
    }
  }

  /**
    * Signature in the sum composition
    * @param sk secret key tree of the sum composition
    * @param m message to be signed
    * @param step  current time step of signing key sk
    * @return byte array signature
    */
  def sumSign(sk: Tree[Array[Byte]],m: Array[Byte],step:Int): Array[Byte] = {
    assert(step == sumGetKeyTimeStep(sk))
    assert(sumVerifyKeyPair(sk,sumGetPublicKey(sk)))
    val stepBytesBigInt = BigInt(step).toByteArray
    val stepBytes = Array.fill(seedBytes-stepBytesBigInt.length){0x00.toByte}++stepBytesBigInt
    //loop that generates the signature of m++step and stacks up the witness path of the key
    def loop(t: Tree[Array[Byte]]): Array[Byte] = {
      t match {
        case n: Node[Array[Byte]] => {
          val left = n.l match {
            case nn: Node[Array[Byte]] => {
              loop(nn)
            }
            case ll: Leaf[Array[Byte]] => {
              sSign(m++stepBytes,ll.v.slice(0,skBytes))++ll.v.slice(skBytes,skBytes+pkBytes)++stepBytes
            }
            case _ => Array()
          }
          val right = n.r match {
            case nn: Node[Array[Byte]] => {
              loop(nn)
            }
            case ll: Leaf[Array[Byte]] => {
              sSign(m++stepBytes,ll.v.slice(0,skBytes))++ll.v.slice(skBytes,skBytes+pkBytes)++stepBytes
            }
            case _ => Array()
          }
          left++right++n.v.slice(seedBytes,seedBytes+2*pkBytes)
        }
        case l: Leaf[Array[Byte]] => {
          sSign(m++stepBytes,l.v.slice(0,skBytes))++l.v.slice(skBytes,skBytes+pkBytes)++stepBytes++FastCryptographicHash(l.v.slice(skBytes,skBytes+pkBytes))++FastCryptographicHash(l.v.slice(skBytes,skBytes+pkBytes))
        }
        case _ => {
          Array()
        }
      }
    }
    loop(sk)
  }

  /**
    * Verify in the sum composition
    * @param pk public key of the sum composition
    * @param m message corresponding to the signature
    * @param sig signature to be verified
    * @return true if the signature is valid false if otherwise
    */
  def sumVerify(pk: Array[Byte],m: Array[Byte],sig: Array[Byte]): Boolean = {
    val pkSeq = sig.drop(sigBytes+pkBytes+seedBytes)
    val stepBytes = sig.slice(sigBytes+pkBytes,sigBytes+pkBytes+seedBytes)
    val step = BigInt(stepBytes)
    var pkLogic = true
    if (step % 2 == 0) {
      pkLogic &= FastCryptographicHash(sig.slice(sigBytes,sigBytes+pkBytes)).deep == pkSeq.slice(0,pkBytes).deep
    } else {
      pkLogic &= FastCryptographicHash(sig.slice(sigBytes,sigBytes+pkBytes)).deep == pkSeq.slice(pkBytes,2*pkBytes).deep
    }
    for (i <- 0 to pkSeq.length/pkBytes-4 by 2) {
      val pk0:Array[Byte] = pkSeq.slice((i+2)*pkBytes,(i+3)*pkBytes)
      val pk00:Array[Byte] = pkSeq.slice(i*pkBytes,(i+1)*pkBytes)
      val pk01:Array[Byte] = pkSeq.slice((i+1)*pkBytes,(i+2)*pkBytes)
      val pk1:Array[Byte] = pkSeq.slice((i+3)*pkBytes,(i+4)*pkBytes)
      val pk10:Array[Byte] = pkSeq.slice(i*pkBytes,(i+1)*pkBytes)
      val pk11:Array[Byte] = pkSeq.slice((i+1)*pkBytes,(i+2)*pkBytes)
      if((step.toInt/exp(i/2+1)) % 2 == 0) {
        pkLogic &= pk0.deep == FastCryptographicHash(pk00++pk01).deep
      } else {
        pkLogic &= pk1.deep == FastCryptographicHash(pk10++pk11).deep
      }
    }
    pkLogic &= pk.deep == FastCryptographicHash(pkSeq.slice(pkSeq.length-2*pkBytes,pkSeq.length)).deep
    sVerify(m++stepBytes,sig.slice(0,sigBytes),sig.slice(sigBytes,sigBytes+pkBytes)) && pkLogic
  }

  /**
    * Get the current time step of a sum composition key
    * @param key binary tree key
    * @return time step
    */
  def sumGetKeyTimeStep(key: Tree[Array[Byte]]): Int = {
    key match {
      case n: Node[Array[Byte]] => {
        val left = n.l match {
          case n: Node[Array[Byte]] => {sumGetKeyTimeStep(n)}
          case l: Leaf[Array[Byte]] => {0}
          case _ => 0
        }
        val right = n.r match {
          case n: Node[Array[Byte]] => {sumGetKeyTimeStep(n)+exp(n.height)}
          case l: Leaf[Array[Byte]] => {1}
          case _ => 0
        }
        left+right
      }
      case l: Leaf[Array[Byte]] => 0
      case _ => 0
    }
  }

  /**
    * Generate key in the MMM composition
    * @param seed input entropy for key generation
    * @return
    */
  def generateKey(seed: Array[Byte]): MalkinKey = {
    val r = PRNG(seed)
    val rp = PRNG(r._2)
    //super-scheme sum composition
    val L = sumGenerateKey(r._1,logl)
    //sub-scheme sum composition
    val Si = sumGenerateKey(rp._1,0)
    val pki = sumGetPublicKey(Si)
    val sig = sumSign(L,pki,0)
    assert(sumVerify(sumGetPublicKey(L),pki,sig))
    (L,Si,sig,pki,rp._2)
  }

  /**
    * Updates the key in the MMM composition (product composition with increasing height for
    * Si as L increments)
    * @param key  MMM key to be updated
    * @param t time step key is to be updated to
    * @return updated MMM key
    */
  def updateKey(key: MalkinKey,t:Int): MalkinKey = {
    val keyTime = getKeyTimeStep(key)
    var L = key._1
    var Si = key._2
    var sig = key._3
    var pki = key._4
    var seed = key._5
    val Tl = exp(L.height)
    var Ti = exp(Si.height)
    var tl = sumGetKeyTimeStep(L)
    var ti = sumGetKeyTimeStep(Si)
    if (keyTime < t) {
      for(i <- keyTime+1 to t) {
        tl = sumGetKeyTimeStep(L)
        ti = sumGetKeyTimeStep(Si)
        if (ti+1 < Ti) {
          Si = sumUpdate(Si, ti + 1)
        } else if (tl < Tl) {
          val r = PRNG(seed)
          Si = sumGenerateKey(r._1, tl + 1)
          pki = sumGetPublicKey(Si)
          seed = r._2
          Ti = exp(Si.height)
          L = sumUpdate(L, tl + 1)
          tl = sumGetKeyTimeStep(L)
          sig = sumSign(L,pki,tl)
        } else {
          println("Error: max time steps reached")
        }
      }
    }
    (L,Si,sig,pki,seed)
  }

  /**
    * Fast version on updateKey, should be equivalent input and output
    * @param key
    * @param t
    * @return  updated key
    */
  def updateKeyFast(key: MalkinKey,t:Int): MalkinKey = {
    val keyTime = getKeyTimeStep(key)
    var L = key._1
    var Si = key._2
    var sig = key._3
    var pki = key._4
    var seed = key._5
    val Tl = exp(L.height)
    var Ti = exp(Si.height)
    var tl = sumGetKeyTimeStep(L)
    var ti = sumGetKeyTimeStep(Si)
    if (keyTime < t) {
      var i = keyTime+1
      while(i < t) {
        tl = sumGetKeyTimeStep(L)
        ti = sumGetKeyTimeStep(Si)
        if (t-i > exp(tl)-ti) {
          val r = PRNG(seed)
          seed = r._2
          L = sumUpdate(L, tl + 1)
          tl = sumGetKeyTimeStep(L)
          sig = sumSign(L,pki,tl)
        } else {
          if (ti+1 < Ti) {
            Si = sumUpdate(Si, ti + 1)
          } else if (tl < Tl) {
            val r = PRNG(seed)
            Si = sumGenerateKey(r._1, tl + 1)
            pki = sumGetPublicKey(Si)
            seed = r._2
            Ti = exp(Si.height)
            L = sumUpdate(L, tl + 1)
            tl = sumGetKeyTimeStep(L)
            sig = sumSign(L,pki,tl)
          } else {
            println("Error: max time steps reached")
          }
        }
        i+=1
      }
    } else {
      println("Error: t less than given keyTime")
    }
    (L,Si,sig,pki,seed)
  }

  /**
    * Get the current time step of an MMM key
    * @param key MMM key to be inspected
    * @return Current time step of key
    */
  def getKeyTimeStep(key: MalkinKey): Int = {
    val L = key._1
    val Si = key._2
    val tl = sumGetKeyTimeStep(L)
    val ti = sumGetKeyTimeStep(Si)
    exp(tl)-1+ti
  }

  /**
    * Signature in the MMM composition
    * @param key signing secret key
    * @param m message to be signed
    * @return signature of m
    */
  def sign(key: MalkinKey,m: Array[Byte]): MalkinSignature = {
    val keyTime = BigInt(getKeyTimeStep(key)).toByteArray
    val L = key._1
    val Si = key._2
    val sigi = key._3
    val pki = key._4
    val seed = key._5
    val ti = sumGetKeyTimeStep(Si)
    val tl = sumGetKeyTimeStep(L)
    val sigm = sumSign(Si,m++keyTime,ti)
    (sigi,sigm,pki)
  }

  /**
    * Verify MMM signature
    * @param pk public key of the MMM secret key
    * @param m message corresponding to signature
    * @param sig signature to be verified
    * @return true if signature is valid false if otherwise
    */
  def verify(pk: Array[Byte],m: Array[Byte],sig: MalkinSignature,t: Int): Boolean = {
    val sigi = sig._1
    val sigm = sig._2
    val pki = sig._3
    val stepL = BigInt(sigi.slice(sigBytes+pkBytes,sigBytes+pkBytes+seedBytes)).toInt
    val stepSi = BigInt(sigm.slice(sigBytes+pkBytes,sigBytes+pkBytes+seedBytes)).toInt
    sumVerify(pk,pki,sigi) && sumVerify(pki,m++BigInt(t).toByteArray,sigm) && (t==exp(stepL)-1+stepSi)
  }

  /**
    * Get the public key of an MMM private key
    * @param key
    * @return
    */
  def publicKey(key: MalkinKey):  Array[Byte] = {
    sumGetPublicKey(key._1)
  }

}
