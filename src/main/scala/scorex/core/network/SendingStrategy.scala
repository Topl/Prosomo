package scorex.core.network

import scala.util.Random

trait SendingStrategy {
  def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer]
}

object SendToRandom extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = {
    if (peers.nonEmpty) {
      Seq(peers(Random.nextInt(peers.length)))
    } else {
      Seq.empty
    }
  }
}

case object Broadcast extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = peers
}

case class BroadcastExceptOf(exceptOf: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    peers.filterNot(exceptOf.contains)
}

case class BroadcastExceptOfByName(exceptOf: String) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    peers.filterNot(p => p.peerInfo.get.peerSpec.agentName == exceptOf)
}

case class SendToPeer(chosenPeer: ConnectedPeer) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = Seq(chosenPeer)
}

case class SendToPeerByName(chosenPeer: String) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = {
    val out = peers.filter(p => p.peerInfo.get.peerSpec.agentName == chosenPeer )
    if (out.isEmpty) {
      println("Error: no peer found by name "+chosenPeer)
      println("All peers:")
      peers.foreach(p=>println("_"+p.peerInfo.get.peerSpec.agentName+"_","_"+chosenPeer+"_",p.peerInfo.get.peerSpec.agentName == chosenPeer))
    }
    out
  }
}

case class SendToPeers(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = chosenPeers
}

case class SendToRandomFromChosen(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    Seq(chosenPeers(Random.nextInt(chosenPeers.length)))
}
