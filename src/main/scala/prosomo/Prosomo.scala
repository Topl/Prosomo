package prosomo

import akka.actor.ActorSystem
import prosomo.cases._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.StdIn
import bifrost.BifrostApp
import com.typesafe.sslconfig.ssl.SSLParametersConfig


class Prosomo(override val settingsFilename: String) extends BifrostApp(settingsFilename) {
  /**
    * Ouroboros Prosomoiotís:
    *
    * Dynamic proof of stake protocol simulated with akka actors
    * based on Praos and Genesis revisions of Ouroboros
    *
    */

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

object Prosomo extends App {
  val input = args
  val config = new ParameterConfig
  new Prosomo(config.settingsFilename).run()
}
