package bifrost.network.peer

import java.net.InetSocketAddress

import bifrost.settings.Settings
import bifrost.utils.ScorexLogging

import scala.collection.mutable


//todo: persistence
class PeerDatabaseImpl(settings: Settings, filename: Option[String]) extends PeerDatabase with ScorexLogging {

  private val whitelistPersistence = mutable.Map[InetSocketAddress, PeerInfo]()

  private val blacklist = mutable.Map[String, Long]()

  private lazy val ownNonce = settings.nodeNonce

  def clear(remote:InetSocketAddress): Unit = {
    whitelistPersistence.get(remote) match {
      case Some(info:PeerInfo) => {
        info.nodeName match {
          case Some(str:String) if str == "declared" => whitelistPersistence -= remote
          case _ =>
        }
      }
      case _ =>
    }
  }

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
//    println(prosomo.primitives.Parameters.myAddress)
//    println(address)
//    println(address.getAddress)
//    println(address.getHostName)
//    println(address.getHostString)
//    println(address.getPort)
//    println(peerInfo.nonce)
//    println(peerInfo.nodeName)
    def addPeer:Unit = {
      whitelistPersistence.get(address) match {
        case Some(info:PeerInfo) => {
          info.nonce match {
            case Some(nonce:Long) if nonce == settings.nodeNonce =>
            case _ => {
              val updatedPeerInfo = whitelistPersistence.get(address).map { dbPeerInfo =>
                val nonceOpt = peerInfo.nonce.orElse(dbPeerInfo.nonce)
                val nodeNameOpt = peerInfo.nodeName.orElse(dbPeerInfo.nodeName)
                PeerInfo(peerInfo.lastSeen, nonceOpt, nodeNameOpt)
              }.getOrElse(peerInfo)
              whitelistPersistence.put(address, updatedPeerInfo)
            }
          }
        }
        case _ => {
          val updatedPeerInfo = whitelistPersistence.get(address).map { dbPeerInfo =>
            val nonceOpt = peerInfo.nonce.orElse(dbPeerInfo.nonce)
            val nodeNameOpt = peerInfo.nodeName.orElse(dbPeerInfo.nodeName)
            PeerInfo(peerInfo.lastSeen, nonceOpt, nodeNameOpt)
          }.getOrElse(peerInfo)
          whitelistPersistence.put(address, updatedPeerInfo)
        }
      }
    }
    peerInfo.nonce match {
      case Some(nonce:Long) if nonce != ownNonce => {
        peerInfo.nodeName match {
          case Some(str:String) if str == "declared" => addPeer
          case _ =>
        }
      }
      case None => {
        peerInfo.nodeName match {
          case Some(str:String) if str == "bootstrap" => addPeer
          case _ =>
        }
      }
      case _ =>
    }
  }

  override def blacklistPeer(address: InetSocketAddress): Unit = {
    whitelistPersistence.remove(address)
    if (!isBlacklisted(address)) blacklist += address.getHostName -> System.currentTimeMillis()
  }

  override def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getHostName))
  }

  override def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo] = {
//    println(whitelistPersistence.keySet.size)
//    whitelistPersistence.keySet.map(_.toString).foreach(println(_))
    (excludeSelf match {
      case true => knownPeers(false).filter(_._2.nonce.getOrElse(-1) != ownNonce)
      case false => whitelistPersistence.keys.flatMap(k => whitelistPersistence.get(k).map(v => k -> v))
    }).toMap
  }

  override def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  override def isEmpty(): Boolean = whitelistPersistence.isEmpty

  def print:Unit = {
    for (entry<-whitelistPersistence) {
      log.info("Known Peer: "+entry._1.toString+" "+entry._2.toString)
    }
  }
}
