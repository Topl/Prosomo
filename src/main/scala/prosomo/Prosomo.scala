package prosomo

import java.awt.{Color, Dimension}

import prosomo.primitives.Parameters.{devMode, inputSeed, messageSpecs, useGui}
import prosomo.primitives.{ColorTextArea, FastCryptographicHash, SharedData}
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
import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork
import com.typesafe.config.{Config, ConfigFactory}
import javax.swing.BorderFactory
import prosomo.cases.GuiCommand

import scala.concurrent.ExecutionContext
import scala.swing.Font.Style
import scala.swing._
import scala.util.Try

class Prosomo(config:Config,window:Option[ProsomoWindow]) extends Runnable with ScorexLogging {

  var runApp = true

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

  window match {
    case None =>
    case Some(win) => Try{
      win.connectButton.get.text = "Connected"
      win.window.get.reactions += {
        case event.WindowClosed(_) => {
          this.stopAll()
          window.get.runApp = false
          runApp = false
          System.setOut(SharedData.oldOut)
        }
      }
      win.cmdButton.get.reactions += {
        case event.ButtonClicked(_) =>
          coordinator ! GuiCommand(win.cmdField.get.text)
      }
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
        //log.error("Unexpected shutdown")
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
  val input = args
  var instance:Option[Prosomo] = None
  if (useGui) {
    val newWindow = new ProsomoWindow(prosomo.primitives.Parameters.config)
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
          while (newWindow.runApp) {
            Thread.sleep(100)
          }
          instance.get.stopAll()
        }
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
  System.exit(0)
}

class ProsomoWindow(config:Config) {

  var windowConfig:Config = config
  var waitToConnect = true
  var runApp = true

  val cmdField = Try{
    new TextField {
      text = ""
      columns = 20
      editable = true
      maximumSize = new Dimension(2000,50)
    }
  }.toOption

  val cmdButton = Try{
    new Button("Issue Command") {}
  }.toOption

  val commandElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += cmdField.get
      contents += cmdButton.get
    }
  }.toOption

  val backgroundC = Color.getHSBColor(1.0.toFloat,0.0.toFloat,0.15.toFloat)
  val foregroundC = Color.getHSBColor(0.46.toFloat,0.6.toFloat,0.7.toFloat)

  var peerList = Try{
    new ListView(SharedData.peerSeq) {
      font = swing.Font("Monospaced",Style.Plain,14)
      renderer = ListView.Renderer(entry=>{
        val padlen = 30
        var out = entry.padTo(60,' ').take(60)
        if (SharedData.stakingAlpha.isDefinedAt(entry.trim)) {
          out += f"Epoch Stake ${SharedData.stakingAlpha(entry.trim)}%1.8f".padTo(padlen,' ').take(padlen)
        }
        if (SharedData.stakingBalance.isDefinedAt(entry.trim)) {
          out += s"Net ${SharedData.stakingBalance(entry.trim)}".padTo(padlen,' ').take(padlen)
        }
        if (SharedData.confirmedAlpha.isDefinedAt(entry.trim)) {
          out += f"Confirmed Stake ${SharedData.confirmedAlpha(entry.trim)}%1.8f".padTo(padlen,' ').take(padlen)
        }
        if (SharedData.confirmedBalance.isDefinedAt(entry.trim)) {
          out += s"Net ${SharedData.confirmedBalance(entry.trim)}".padTo(padlen,' ').take(padlen)
        }
        out
      })
      background = backgroundC
      foreground = foregroundC
    }
  }.toOption

  val peerListElem = Try{
    new ScrollPane(peerList.get) {
      maximumSize = new Dimension(2000,2000)
      preferredSize = new Dimension(800,400)
      minimumSize = new Dimension(100,100)
    }
  }.toOption

  val knownAddressField = Try{
    new TextField {
      text = "35.192.11.126:9084"
      columns = 20
      editable = true
      maximumSize = new Dimension(2000,50)
    }
  }.toOption

  val declaredAddressField = Try{
    new TextField {
      text = Try{windowConfig.getString("scorex.network.declaredAddress")}.toOption match {
        case Some(adr) if adr != "" => adr
        case None => ""
      }
      columns = 20
      editable = true
      maximumSize = new Dimension(2000,50)
    }
  }.toOption

  val upnpCheck = Try{
    new CheckBox() {
      text = "UPNP"
    }
  }.toOption

  val connectButton = Try{
    new Button("Connect") {
      reactions += {
        case event.ButtonClicked(_) =>
          val useUpnp = if (upnpCheck.get.selected) {"yes"} else {"no"}
          val knownPeer = "\""+knownAddressField.get.text+"\""
          val declaredAddress = if (upnpCheck.get.selected) {
            "\"\""
          } else {
            "\""+declaredAddressField.get.text+"\""
          }
          val str = s"input{scorex{network{upnpEnabled=$useUpnp,knownPeers=[$knownPeer],declaredAddress=$declaredAddress}}}"
          windowConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(windowConfig)
          upnpCheck.get.enabled = false
          waitToConnect = false
      }
    }
  }.toOption

  val connectElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += new TextField(" Bootstrap address: ") {editable=false;border=BorderFactory.createEmptyBorder()}
      contents += knownAddressField.get
      contents += new TextField("  Declared address: ") {editable=false;border=BorderFactory.createEmptyBorder()}
      contents += declaredAddressField.get
      contents += upnpCheck.get
      contents += connectButton.get
    }
  }

  val outputText = Try{
    new ColorTextArea {
      editable = false
      font = swing.Font("Monospaced",Style.Plain,14)
      background = backgroundC
      foreground = foregroundC
      lineWrap = true
    }
  }.toOption

  val outputElem = Try{
    new ScrollPane(outputText.get) {
      verticalScrollBar.value = verticalScrollBar.maximum
      maximumSize = new Dimension(2000,2000)
      preferredSize = new Dimension(800,400)
      minimumSize = new Dimension(100,100)
    }
  }.toOption

  val window:Option[Frame] = Try{
    new Frame {
      /*
      GUI additions:
        - network stats, number of peers, active stake
        - basic wallet stats, transaction issue dialog box
        - tine qualities, block time
        - secure key creation
       */
      reactions += {
        case event.WindowClosing(_) => {
          waitToConnect = false
          runApp = false
          System.setOut(SharedData.oldOut)
          prosomo.primitives.Parameters.useGui = false
        }
      }
      title = "Prosomo 0.7"
      iconImage = toolkit.getImage("src/main/resources/Logo.png")

      contents = new BoxPanel(Orientation.Vertical) {
        border = Swing.EmptyBorder(10, 10, 10, 10)
        if (devMode) contents += commandElem.get
        contents += connectElem.get
        contents += peerListElem.get
        contents += outputElem.get
      }
      pack()
      centerOnScreen()
      open()
    }
  }.toOption

  window match {
    case None => Try{
      prosomo.primitives.Parameters.useGui = false
    }
    case Some(frame) => Try{
      while (waitToConnect) {
        Thread.sleep(100)
      }
      if (runApp) {
        connectButton.get.enabled = false
        connectButton.get.text = "Connecting..."
        knownAddressField.get.editable = false
        declaredAddressField.get.editable = false
        outputText.get.text = "Loading..."
        System.setOut(SharedData.printStream)
      }
    }
  }
}