package prosomo.stakeholder

import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Future

//wrapper class for akka ActorRef, remote and local actors handled accordingly

case class ActorRef(
                     routerRef:akka.actor.ActorRef,
                     actorRef:akka.actor.ActorRef,
                     actorPath:akka.actor.ActorPath,
                     remote:Boolean = false
                   ) {

  def canEqual(a:Any):Boolean = a.isInstanceOf[ActorRef]

  override def equals(that: Any):Boolean = that match {
      case that: ActorRef => {
        that.canEqual(this) &&
        this.actorRef == that.actorRef &&
        this.actorPath == that.actorPath &&
        this.routerRef == that.routerRef &&
        this.remote == that.remote
      }
      case _=> false
    }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (actorRef != null) 0 else actorRef.hashCode)
    result = prime * result + (if (actorPath != null) 0 else actorPath.hashCode)
    result = prime * result + (if (routerRef != null) 0 else routerRef.hashCode)
    result = prime * result + remote.hashCode
    result
  }

  def path:akka.actor.ActorPath = actorPath

  def ! (that:Any)(implicit sender: akka.actor.ActorRef = akka.actor.Actor.noSender):Unit = if (!this.remote) {
    this.actorRef ! that
  } else {
    routerRef ! (actorPath,that)
  }
  def ? (that:Any)(implicit timeout: Timeout, sender: akka.actor.ActorRef = akka.actor.Actor.noSender):Future[Any] = if (!this.remote) {
    this.actorRef ? that
  } else {
    routerRef ? (actorPath,that)
  }

}

object ActorRef {

  def apply(actorRef:akka.actor.ActorRef)(implicit routerRef:ActorRef):ActorRef = {
    new ActorRef(actorRef = actorRef ,actorPath = actorRef.path,remote = false,routerRef = routerRef.actorRef)
  }


  def apply(path:akka.actor.ActorPath)(implicit routerRef:ActorRef):ActorRef = {
    new ActorRef(actorPath = path, routerRef = routerRef.actorRef, remote = true, actorRef = routerRef.actorRef )
  }

  def routerRef(actorRef:akka.actor.ActorRef) = new ActorRef(actorRef,actorRef,actorRef.path,false)
}