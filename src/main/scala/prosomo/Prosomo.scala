package prosomo

import java.lang.management.ManagementFactory

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import bifrost.{BifrostLocalInterface, BifrostNodeViewHolder}
import bifrost.api.http._
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.history.BifrostSyncInfoMessageSpec
import bifrost.network.message.{MessageSpec, _}
import bifrost.network.peer.PeerManager
import bifrost.network.{BifrostNodeViewSynchronizer, NetworkController, UPnP}
import bifrost.utils.ScorexLogging
import com.sun.management.HotSpotDiagnosticMXBean
import io.circe
import prosomo.cases._
import prosomo.primitives.Parameters.inputSeed
import prosomo.stakeholder.{Coordinator, Router}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.{Type, _}

class Prosomo(settingsFilename: String) extends Runnable with ScorexLogging {

  val ApplicationNameLimit = 50

  val settings:ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  log.debug(s"Starting application with settings \n$settings")

  implicit val actorSystem:ActorSystem = ActorSystem(settings.agentName)

  val additionalMessageSpecs: Seq[MessageSpec[_]] =
    Seq(BifrostSyncInfoMessageSpec)

  val upnp:UPnP = new UPnP(settings)

  val basicSpecs = Seq(GetPeersSpec,PeersSpec,InvSpec,RequestModifierSpec,ModifiersSpec)

  val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)

  println("Using seed: "+inputSeed)

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

  val peerManagerRef:ActorRef = actorSystem.actorOf(Props(classOf[PeerManager], settings))

  val networkController:ActorRef = actorSystem.actorOf(Props(classOf[NetworkController], settings, messagesHandler, upnp, peerManagerRef), "networkController")

  val routerRef:ActorRef = actorSystem.actorOf(Router.props(FastCryptographicHash(inputSeed+"router"),Seq(networkController)), "Router")
  val coordinator:ActorRef = actorSystem.actorOf(Coordinator.props(FastCryptographicHash(inputSeed),Seq(routerRef)), "Coordinator")
  coordinator ! NewDataFile
  coordinator ! Populate
  coordinator ! Register
  coordinator ! Run

  val apiRoutes:Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, routerRef),
    WalletApiRoute(settings, routerRef),
    ProgramApiRoute(settings, routerRef, networkController),
    AssetApiRoute(settings, routerRef),
    UtilsApiRoute(settings),
    NodeViewApiRoute(settings, routerRef)
  )

  val apiTypes:Seq[Type] = Seq(typeOf[UtilsApiRoute],
    typeOf[DebugApiRoute],
    typeOf[WalletApiRoute],
    typeOf[ProgramApiRoute],
    typeOf[AssetApiRoute],
    typeOf[NodeViewApiRoute])

  val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute

  val vm_version = System.getProperty("java.vm.version")
  System.out.printf("java.vm.version = %s%n", vm_version)

  val bean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])

  val enableJVMCI = bean.getVMOption("EnableJVMCI")
  System.out.println(enableJVMCI)

  val useJVMCICompiler = bean.getVMOption("UseJVMCICompiler")
  System.out.println(useJVMCICompiler)

  val compiler = System.getProperty("jvmci.Compiler")
  System.out.printf("jvmci.Compiler = %s%n", compiler)

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

}

object Prosomo extends App {
  val input = args
  new Prosomo(prosomo.primitives.Parameters.settingsFilename).run()
}
