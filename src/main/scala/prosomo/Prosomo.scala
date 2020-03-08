package prosomo

import java.lang.management.ManagementFactory

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import bifrost.api.http._
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostSyncInfoMessageSpec
import bifrost.network.message.{MessageSpec, _}
import bifrost.network.peer.PeerManager
import bifrost.network.{NetworkController, UPnP}
import bifrost.utils.ScorexLogging
import com.sun.management.HotSpotDiagnosticMXBean
import io.circe
import prosomo.cases._
import prosomo.primitives.Parameters.inputSeed
import prosomo.stakeholder.Coordinator

import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn
import scala.reflect.runtime.universe.{Type, _}

class Prosomo(settingsFilename: String) extends Runnable with ScorexLogging {

  val ApplicationNameLimit = 50

  val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  log.debug(s"Starting application with settings \n$settings")

  implicit val actorSystem = ActorSystem(settings.agentName)

  val additionalMessageSpecs: Seq[MessageSpec[_]] =
    Seq(BifrostSyncInfoMessageSpec)

  val upnp = new UPnP(settings)

  val basicSpecs = Seq(GetPeersSpec,PeersSpec,InvSpec,RequestModifierSpec,ModifiersSpec)

  val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)

  println("Using seed: "+inputSeed)
  val coordinator = actorSystem.actorOf(Coordinator.props(FastCryptographicHash(inputSeed)), "Coordinator")
  coordinator ! NewDataFile
  coordinator ! Populate
  coordinator ! Run

  //val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))

  //val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef))

  //val localInterface: ActorRef = actorSystem.actorOf(
  //  Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings)
  //)

  //val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
  //  Props(classOf[BifrostNodeViewSynchronizer],
  //    networkController,
  //    nodeViewHolderRef,
  //    localInterface,
  //    BifrostSyncInfoMessageSpec)
  //)

  val peerManagerRef = actorSystem.actorOf(Props(classOf[PeerManager], settings))

  val nProps = Props(classOf[NetworkController], settings, messagesHandler, upnp, peerManagerRef)
  val networkController = actorSystem.actorOf(nProps, "networkController")

  val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, coordinator),
    WalletApiRoute(settings, coordinator),
    ProgramApiRoute(settings, coordinator, networkController),
    AssetApiRoute(settings, coordinator),
    UtilsApiRoute(settings),
    NodeViewApiRoute(settings, coordinator)
  )

  val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute],
    typeOf[DebugApiRoute],
    typeOf[WalletApiRoute],
    typeOf[ProgramApiRoute],
    typeOf[AssetApiRoute],
    typeOf[NodeViewApiRoute])

  val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute

  def run(): Unit = {
    require(settings.agentName.length <= ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at 0.0.0.0:${settings.rpcPort}")

    implicit val materializer:ActorMaterializer = ActorMaterializer()
    Http().bindAndHandle(combinedRoute, "0.0.0.0", settings.rpcPort)

    //on unexpected shutdown
    val newThread = new Thread() {
      override def run() {
        log.error("Unexpected shutdown")
        stopAll()
      }
    }
    Runtime.getRuntime.addShutdownHook(newThread)
  }

  def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    if (settings.upnpEnabled) upnp.deletePort(settings.port)
    networkController ! NetworkController.ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>

      log.info("Exiting from the app...")
      System.exit(0)
    }
  }

  val vm_version = System.getProperty("java.vm.version")
  System.out.printf("java.vm.version = %s%n", vm_version)

  val bean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])

  val enableJVMCI = bean.getVMOption("EnableJVMCI")
  System.out.println(enableJVMCI)

  val useJVMCICompiler = bean.getVMOption("UseJVMCICompiler")
  System.out.println(useJVMCICompiler)

  val compiler = System.getProperty("jvmci.Compiler")
  System.out.printf("jvmci.Compiler = %s%n", compiler)

  if (true) {
    println("-->Press ENTER to exit<--")
    try StdIn.readLine()
    finally {
      stopAll()
    }
  }
}

object Prosomo extends App {
  val input = args
  new Prosomo(prosomo.primitives.Parameters.settingsFilename).run()
}
