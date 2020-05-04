package bifrost.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Timers}
import akka.pattern.ask
import akka.util.Timeout
import bifrost.network.NetworkController.{DataFromPeer, SendToNetwork}
import bifrost.network.message.{GetPeersSpec, Message, PeersSpec}
import bifrost.network.peer.PeerManager
import bifrost.network.peer.PeerManager.{GetAllPeers, KnownPeers, RandomPeers}
import bifrost.utils.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


class PeerSynchronizer(val networkControllerRef: ActorRef, peerManager: ActorRef) extends Actor with Timers with ScorexLogging {

  private implicit val timeout = Timeout(5.seconds)

  val messageSpecs = Seq(GetPeersSpec, PeersSpec)

  case object TimerKey

  val msg = Message[Unit](GetPeersSpec, Right(Unit), None)
  val stn = NetworkController.SendToNetwork(msg, Broadcast)

  override def preStart: Unit = {
    super.preStart()

    networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)
    context.system.scheduler.schedule(2.seconds, 10.seconds)(networkControllerRef ! stn)
    timers.startPeriodicTimer(TimerKey,TimerKey,10.seconds)
  }

  override def receive: Receive = {
    case DataFromPeer(spec, peers: Seq[InetSocketAddress]@unchecked, remote)
      if spec.messageCode == PeersSpec.messageCode && peers.cast[Seq[InetSocketAddress]].isDefined => {
      peers.foreach(isa => peerManager ! PeerManager.AddOrUpdatePeer(isa, None, Some("declared")))
    }

    case DataFromPeer(spec, _, remote) if spec.messageCode == GetPeersSpec.messageCode => {
      peerManager ! GetAllPeers(remote,networkControllerRef)
    }

    case TimerKey => {
      networkControllerRef ! stn
    }

    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}
