package prosomo

import java.net.InetSocketAddress
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import akka.stream.ActorMaterializer
import com.typesafe.config.Config
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.{GuiCommand, IssueTxToAddress}
import prosomo.primitives.Parameters.{inputSeed, messageSpecs, useGui}
import prosomo.primitives.{Fch, SharedData}
import prosomo.stakeholder.{Coordinator, Router}
import scorex.core.api.http.{ApiErrorHandler, ApiRejectionHandler, ApiRoute, CompositeHttpService}
import scorex.core.app.{Application, ScorexContext}
import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.network.peer.PeerManagerRef
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging
import scorex.util.encode.Base58
import scala.concurrent.ExecutionContext
import scala.swing._
import scala.util.Try

/**
  * AMS 2020:
  * The Prosomo testnet runtime
  * The App instantiates this class with a given configuration with user defined inputs
  * The actor system in this class is interfaced with the GUI element so buttons can trigger messages to be passed
  * Coordinator is the only actor ref that should receive messages from GUI reaction events
  * @param config config to start the node
  * @param window optional GUI element
  */

class Prosomo(config:Config,window:Option[ProsomoWindow]) extends Runnable with ScorexLogging {
  val fch = new Fch
  var runApp = true

  //settings
  implicit val settings: ScorexSettings = ScorexSettings.fromConfig(config)
  SharedData.scorexSettings = Some(settings)

  //api
  val apiRoutes: Seq[ApiRoute] = Seq()

  implicit def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler
  implicit def rejectionHandler: RejectionHandler = ApiRejectionHandler.rejectionHandler

  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.nodeName)
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("scorex.executionContext")

  protected val features: Seq[PeerFeature] = Seq()
  protected val additionalMessageSpecs: Seq[MessageSpec[_]] = messageSpecs

  //p2p
  private val upnpGateway: Option[UPnPGateway] = if (settings.network.upnpEnabled) UPnP.getValidGateway(settings.network) else None
  // TODO use available port on gateway instead settings.network.bindAddress.getPort
  upnpGateway.foreach(_.addPort(settings.network.bindAddress.getPort))

  private lazy val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    val modifiersSpec = new ModifiersSpec(settings.network.maxPacketSize)
    val featureSerializers: PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap
    Seq(
      GetPeersSpec,
      new PeersSpec(featureSerializers, settings.network.maxPeerSpecObjects),
      invSpec,
      requestModifierSpec,
      modifiersSpec
    )
  }

  /**
    * Note AMS June 2020: This time NTP time provider is not used in consensus and is to be only used for logging and network information,
    * a separate NTP functionality with additional security properties and resilience is to be developed for tracking the global slot in Ouroboros Genesis
    */
  val timeProvider = new NetworkTimeProvider(settings.ntp)

  //an address to send to peers
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    settings.network.declaredAddress orElse {
      // TODO use available port on gateway instead settings.bindAddress.getPort
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, settings.network.bindAddress.getPort))
    }
  }

  val scorexContext = ScorexContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    features = features,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  val peerManagerRef = PeerManagerRef(settings, scorexContext)

  val networkControllerRef: ActorRef = NetworkControllerRef(
    "networkController", settings.network, peerManagerRef, scorexContext)

  val swaggerConfig:String = ""

  lazy val combinedRoute: Route = CompositeHttpService(actorSystem, apiRoutes, settings.restApi, swaggerConfig).compositeRoute

  log.debug(s"Starting application with settings \n$settings")
  log.info("Using seed: "+inputSeed)

  val routerRef:ActorRef = actorSystem.actorOf(Router.props(fch.hash(inputSeed+"router"),Seq(networkControllerRef,peerManagerRef)), "Router")
  val coordinatorRef:ActorRef = actorSystem.actorOf(Coordinator.props(fch.hash(inputSeed),Seq(routerRef)), "Coordinator")

  window match {
    case None =>
    case Some(win) => Try{
      window.get.activePane.get.pages(1).enabled = true
      window.get.activePane.get.pages(2).enabled = true
      window.get.connectButton.get.text = "Connected"
      window.get.window.get.reactions += {
        case event.WindowClosed(_) => {
          this.stopAll()
          window.get.runApp = false
          runApp = false
          System.setOut(SharedData.oldOut)
        }
      }
      window.get.cmdButton.get.reactions += {
        case event.ButtonClicked(_) =>
          coordinatorRef ! GuiCommand(window.get.cmdField.get.text)
      }
      window.get.sendTxButton.get.reactions += {
        case scala.swing.event.ButtonClicked(_) => {
          SharedData.selfWrapper match {
            case Some(actorRefWrapper) =>
              Base58.decode(window.get.txWin.get.recipField.get.text).toOption match {
                case Some(pk) => Try{BigInt(window.get.txWin.get.deltaField.get.text)}.toOption match {
                  case Some(delta) => actorRefWrapper ! IssueTxToAddress(ByteArrayWrapper(pk),delta)
                  case None =>
                }
                case None =>
              }
            case None =>
          }
          window.get.confirmSendToNetworkWindow.get.close()
          window.get.txWin.get.issueTxWindow.get.close()
          window.get.txWin = None
          window.get.issueTxButton.get.enabled = true
        }
      }
      window.get.coordRef = coordinatorRef
    }
  }

  def run(): Unit = {
    require(settings.network.agentName.length <= Application.ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at ${settings.restApi.bindAddress.toString}")

    implicit val materializer: ActorMaterializer = ActorMaterializer()
    val bindAddress = settings.restApi.bindAddress

    Http().bindAndHandle(combinedRoute, bindAddress.getAddress.getHostAddress, bindAddress.getPort)

    //on unexpected shutdown
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        stopAll()
      }
    })
  }

  def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    upnpGateway.foreach(_.deletePort(settings.network.bindAddress.getPort))
    networkControllerRef ! ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")
    actorSystem.terminate().onComplete { _ =>
      log.info("Exiting from the app...")
    }
    runApp = false
  }
}

object Prosomo extends App {
  /**
    * input args will be either HOCON *.conf files in execution directory or HOCON formatted strings that add to base config
    */
  val input = args
  var instance:Option[Prosomo] = None
  if (useGui) {
    val newWindow = new ProsomoWindow(prosomo.primitives.Parameters.config)
    //shared reference to window so stakeholder can enable buttons when started
    SharedData.prosomoWindow = Some(newWindow)
    newWindow.window match {
      case None => {
        instance = Try{new Prosomo(prosomo.primitives.Parameters.config,None)}.toOption
        Try{
          instance.get.run()
          while (instance.get.runApp) {
            Thread.sleep(100)
          }
          instance.get.stopAll()
        }
      }
      case Some(frame) if newWindow.runApp => {
        instance = Try{new Prosomo(newWindow.windowConfig,Some(newWindow))}.toOption
        Try{
          instance.get.run()
          var i = 0
          while (newWindow.runApp) {
            newWindow.refreshOutput
            i+=1
            if (i%100==0) {i=0;newWindow.refreshPeerList;newWindow.refreshWallet}
            Thread.sleep(10)
          }
          instance.get.stopAll()
        } orElse Try{println("Error: instance terminated")}
      }
      case _ =>
    }
  } else {
    instance = Try{new Prosomo(prosomo.primitives.Parameters.config,None)}.toOption
    Try{
      instance.get.run()
      while (instance.get.runApp) {
        Thread.sleep(100)
      }
      instance.get.stopAll()
    }
  }
  Thread.sleep(100)
  System.exit(0)
}