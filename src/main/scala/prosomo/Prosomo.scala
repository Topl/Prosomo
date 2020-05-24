package prosomo

import prosomo.primitives.Parameters.{inputSeed, messageSpecs, useGui}
import prosomo.primitives.{FastCryptographicHash, SharedData}
import prosomo.stakeholder.{Coordinator, Router}
import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import akka.stream.ActorMaterializer
import scorex.core.api.http.{ApiErrorHandler, ApiRejectionHandler, ApiRoute, CompositeHttpService}
import scorex.core.app.{Application, ScorexContext}
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.network.peer.PeerManagerRef
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging
import com.typesafe.config.Config
import prosomo.cases.GuiCommand

import scala.concurrent.ExecutionContext
import scala.swing._

class Prosomo(config:Config) extends Runnable with ScorexLogging {

  import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork

  //settings
  implicit val settings: ScorexSettings = ScorexSettings.fromConfig(config)

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

  val routerRef:ActorRef = actorSystem.actorOf(Router.props(FastCryptographicHash(inputSeed+"router"),Seq(networkControllerRef,peerManagerRef)), "Router")
  val coordinator:ActorRef = actorSystem.actorOf(Coordinator.props(FastCryptographicHash(inputSeed),Seq(routerRef)), "Coordinator")

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
        log.error("Unexpected shutdown")
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
      System.exit(0)
    }
  }
  if (useGui) {
    val window = new Frame {
      title = "Prosomo 0.7"
      contents = new BoxPanel(Orientation.Vertical) {
        val textField = new TextField {
          text = ""
          columns = 20
          editable = true
        }
        contents += textField
        contents += new Button("Issue Command") {
          reactions += {
            case event.ButtonClicked(_) =>
              coordinator ! GuiCommand(textField.text)
          }
        }

        contents += Swing.VStrut(5)
        contents += new ScrollPane(SharedData.peerList)
        border = Swing.EmptyBorder(10, 10, 10, 10)
      }


      pack()
      centerOnScreen()
      open()
    }
  }
}

object Prosomo extends App {
  val input = args
  new Prosomo(prosomo.primitives.Parameters.config).run()
}
