package prosomo

import akka.actor.ActorSystem
import prosomo.cases._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.StdIn
import bifrost.BifrostApp
import prosomo.primitives.Parameters
import prosomo.coordinator.Coordinator


class ProsomoBifrost(override val settingsFilename: String) extends BifrostApp(settingsFilename) {

  val system = actorSystem
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

class Prosomo {

  val system = ActorSystem("prosomo")
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

object Prosomo extends App {
  val input = args
  //new ProsomoBifrost(Parameters.settingsFilename).run()
  new Prosomo
}
