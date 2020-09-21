package prosomo.stakeholder

import java.io.BufferedWriter

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases._
import prosomo.components.{Block, Tine, Transaction}
import prosomo.primitives._
import prosomo.providers.TineProvider
import scorex.util.encode.Base58

import scala.concurrent.duration._
import scala.math.BigInt
import scala.util.{Failure, Random, Success, Try}

/**
  * AMS 2020:
  * The receive statement of Stakeholder,
  * Contains cases of all messages Stakeholder will encounter from local and remote interfaces
  * Message authentication occurs for Block passing and TinePool messages
  */

trait SPReceive extends Members {
  def receive: Receive = {



    case unknown:Any => if (!actorStalled) {
      print("Error: received unknown message ")
      println(unknown.getClass.toString)
    }
  }
}
