package prosomo

import akka.actor.ActorSystem

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.StdIn

object Prosomo extends App {

  /**
    * Ouroboros Prosomoiotís:
    *
    * Dynamic proof of stake protocol simulated with akka actors
    * based on Praos and Genesis revisions of Ouroboros
    *
    */

  val input = args
  val system = ActorSystem("Stakeholders")
  val coordinator = system.actorOf(Coordinator.props, "Coordinator")
  coordinator ! NewDataFile
  coordinator ! Populate
  coordinator ! Run

  if (true) {
    println("-->Press ENTER to exit<--")
    try StdIn.readLine()
    finally {
      system.terminate()
    }
  }

  Await.ready(system.whenTerminated, Duration.Inf)

}
