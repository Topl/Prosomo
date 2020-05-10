package bifrost.network.peer

import java.net.InetSocketAddress

import bifrost.settings.Settings

import scala.collection.mutable


//todo: persistence
class PeerDatabaseImpl(settings: Settings, filename: Option[String]) extends PeerDatabase {

  private val whitelistPersistence = mutable.Map[InetSocketAddress, PeerInfo]()

  private val blacklist = mutable.Map[String, Long]()

  private lazy val ownNonce = settings.nodeNonce

  override def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    if (!address.getHostName.contains(prosomo.primitives.Parameters.myAddress)) {
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
      println(entry._1,entry._2)
    }
  }
}
