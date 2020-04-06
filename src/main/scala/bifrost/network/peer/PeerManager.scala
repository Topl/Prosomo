package bifrost.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import bifrost.network.NetworkController.SendToNetwork
import bifrost.network._
import bifrost.network.message.{Message, PeersSpec}
import bifrost.settings.Settings
import bifrost.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.util.Random
import scala.util.{Try,Success,Failure}

/**
  * Peer manager takes care of peers connected and in process, and also choose a random peer to connect
  * Must be singleton
  */
class PeerManager(settings: Settings) extends Actor with ScorexLogging {

  import PeerManager._

  private val connectedPeers = mutable.Map[ConnectedPeer, Option[Handshake]]()
  private var connectingPeer: Option[InetSocketAddress] = None

  private lazy val peerDatabase = new PeerDatabaseImpl(settings, settings.dataDirOpt.map(_ + "/peers.dat"))

  if (peerDatabase.isEmpty()) {
    settings.knownPeers.foreach { address =>
      val defaultPeerInfo = PeerInfo(System.currentTimeMillis(), None, None)
      peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
    }
    prosomo.primitives.Parameters.knownPeer match {
      case str:String if str != "" => Try{
        //val addrParts = str.split(":")
        val defaultPort = 9084
        //val port = if (addrParts.size == 2) addrParts(1).toInt else defaultPort
        val address = new InetSocketAddress(str, defaultPort)
        val defaultPeerInfo = PeerInfo(System.currentTimeMillis(), None, None)
        peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
      }
      case _ =>
    }
    //val localAddress:InetSocketAddress = new InetSocketAddress(settings.bindAddress,settings.port)
    //val defaultPeerInfo = PeerInfo(System.currentTimeMillis(), Some(settings.nodeNonce), None)
    //peerDatabase.addOrUpdateKnownPeer(localAddress, defaultPeerInfo)
  }

  private def randomPeer(): Option[InetSocketAddress] = {
    val peers = peerDatabase.knownPeers(true).keys.toSeq
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNonceOpt, peerNameOpt) =>
      val peerInfo = PeerInfo(System.currentTimeMillis(), peerNonceOpt, peerNameOpt)
      peerDatabase.addOrUpdateKnownPeer(address, peerInfo)

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers(false).keys.toSeq

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers(false).keys.toSeq).take(howMany)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      sender() ! sendingStrategy.choose(connectedPeers.keys.toSeq)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.flatten.toSeq: Seq[Handshake])

    case value:GetAllPeers =>{
      val allPeers:Seq[InetSocketAddress] = peerDatabase.knownPeers(false).keySet.toSeq
      val msg = Message(PeersSpec, Right(allPeers), None)
      value.networkControllerRef ! SendToNetwork(msg, SendToPeers(Seq(value.remote)))
    }

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklistedPeers()
  }

  private def peerCycle: Receive = {
    case Connected(newPeer@ConnectedPeer(remote, _)) =>
      if (peerDatabase.isBlacklisted(newPeer.socketAddress)) {
        log.info(s"Got incoming connection from blacklisted $remote")
      } else {
        connectedPeers += newPeer -> None
        if (connectingPeer.contains(remote)) {
          log.info(s"Connected to $remote. ${connectedPeers.size} connections are open")
          connectingPeer = None
        } else {
          log.info(s"Got incoming connection from $remote. ${connectedPeers.size} connections are open")
        }
      }

    case Handshaked(address, handshake) =>
      if (peerDatabase.isBlacklisted(address)) {
        log.info(s"Got handshake from blacklisted $address")
      } else {
        val toUpdate = connectedPeers.filter { case (cp, h) =>
          cp.socketAddress == address || h.map(_.nodeNonce == handshake.nodeNonce).getOrElse(true)
        }
//        val defaultPeerInfo = PeerInfo(System.currentTimeMillis(), None, None)
//        println("Adding Peer to Database")
//        peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
        if (toUpdate.isEmpty) {
          log.info("No peer to update")
        } else {
          val newCp = toUpdate
            .find(t => handshake.declaredAddress.contains(t._1.socketAddress))
            .getOrElse(toUpdate.head)
            ._1

          toUpdate.keys.foreach(connectedPeers.remove)

          //drop connection to self if occurred
          if (handshake.nodeNonce == settings.nodeNonce) {
            newCp.handlerRef ! PeerConnectionHandler.CloseConnection
          } else {
            handshake.declaredAddress.foreach(address => self ! PeerManager.AddOrUpdatePeer(address, None, None))
            connectedPeers += newCp -> Some(handshake)
          }
        }
      }

    case Disconnected(remote) =>
      connectedPeers.retain { case (p, _) => p.socketAddress != remote }
      if (connectingPeer.contains(remote)) {
        connectingPeer = None
      }
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections && connectingPeer.isEmpty) {
        randomPeer().foreach { address =>
          if (!connectedPeers.map(_._1.socketAddress).contains(address)) {
            connectingPeer = Some(address)
            sender() ! NetworkController.ConnectTo(address)
          }
        }
      }

    case AddToBlacklist(peer) =>
      log.info(s"Blacklist peer $peer")
      peerDatabase.blacklistPeer(peer)
  }: Receive) orElse peerListOperations orElse apiInterface orElse peerCycle
}

object PeerManager {

  case class AddOrUpdatePeer(address: InetSocketAddress, peerNonce: Option[Long], peerName: Option[String])

  case object KnownPeers

  case object RandomPeer

  case class RandomPeers(hawMany: Int)

  case object CheckPeers

  case class Connected(newPeer: ConnectedPeer)

  case class Handshaked(address: InetSocketAddress, handshake: Handshake)

  case class Disconnected(remote: InetSocketAddress)

  case class AddToBlacklist(remote: InetSocketAddress)

  case class FilterPeers(sendingStrategy: SendingStrategy)

  case class GetAllPeers(remote: ConnectedPeer,networkControllerRef:ActorRef)

  case object GetBlacklistedPeers

  case object GetConnectedPeers

}
