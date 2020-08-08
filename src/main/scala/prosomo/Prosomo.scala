package prosomo

import java.io.File
import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import akka.stream.ActorMaterializer
import com.typesafe.config.Config
import io.iohk.iodb.ByteArrayWrapper
import prosomo.cases.{GuiCommand, IssueTxToAddress, NewDataFile, NewHolderFromUI, Populate, Register, Run}
import prosomo.components.Serializer
import prosomo.primitives.{Fch, Kes, KeyFile, Parameters, SharedData, Sig, SystemLoadMonitor, Vrf}
import prosomo.providers.{Coordinator, Router}
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
import scala.util.{Failure, Success, Try}

/**
  * AMS 2020:
  * The Prosomo testnet runtime based on Scorex 2
  * The App instantiates this class with a given configuration with user defined inputs
  * The actor system in this class is interfaced with the GUI element so buttons can trigger messages to be passed
  * Coordinator is the only actor ref that should receive messages from GUI reaction events
  * @param config config to start the node
  * @param window optional GUI element
  */

class Prosomo(config:Config,window:Option[ProsomoWindow]) extends Runnable with ScorexLogging {
  val fch = new Fch
  var runApp = true
  var upnpFailed = false
  implicit val settings: ScorexSettings = ScorexSettings.fromConfig(config)
  SharedData.scorexSettings = Some(settings)
  val apiRoutes: Seq[ApiRoute] = Seq()
  val prosomoMessageSpecs = Parameters.prosomoMessageSpecs
  val inputSeed = Parameters.inputSeedString
  val prosomoNodeUID = Parameters.prosomoNodeUID

  implicit def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler
  implicit def rejectionHandler: RejectionHandler = ApiRejectionHandler.rejectionHandler

  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.nodeName)
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("params.networkController")

  protected val features: Seq[PeerFeature] = Seq()
  protected val additionalMessageSpecs: Seq[MessageSpec[_]] = prosomoMessageSpecs

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
    * Note AMS June 2020: This time NTP time provider is not used in consensus
    * and is to be only used for logging and network information,
    * a separate NTP functionality with additional security properties
    * and resilience is to be developed for tracking the global slot in Ouroboros Genesis
    */
  val timeProvider = new NetworkTimeProvider(settings.ntp)

  /**
    * Note AMS June 2020:
    * The external address that peers will discover,
    * The port range is defined by the Internet Assigned Numbers Authority for ephemeral ports 49152 to 65535
    */
  val upnpGateway: Option[UPnPGateway] = if (settings.network.upnpEnabled) {
    Try{
      UPnP.getValidGateway(settings.network)
    }.toOption match {
      case None =>
        println("Error: UPNP device not found, exiting")
        runApp = false
        upnpFailed = true
        None
      case Some(plug) => plug
    }
  } else {
    None
  }

  val newPort: Int = scala.util.Random.nextInt(65535 - 49152) + 49152

  val externalSocketAddress: Option[InetSocketAddress] = if (settings.network.upnpEnabled) {
    Try{
      upnpGateway.get.addPort(newPort)
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, newPort))
    }.toOption match {
      case None =>
        println("Error: UPNP port mapping failed, exiting")
        runApp = false
        upnpFailed = true
        None
      case Some(adr) => adr
    }
  } else {
    settings.network.declaredAddress
  }

  val scorexContext: ScorexContext = ScorexContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    features = features,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  val peerManagerRef: ActorRef = PeerManagerRef(settings, scorexContext)
  val networkControllerRef: ActorRef = NetworkControllerRef(
    "networkController", settings.network, peerManagerRef, scorexContext)
  val swaggerConfig:String = ""
  lazy val combinedRoute: Route =
    CompositeHttpService(actorSystem, apiRoutes, settings.restApi, swaggerConfig).compositeRoute

  log.info(s"Starting application with settings \n$settings")
  log.info("Using seed: "+inputSeed)
  val routerRef:ActorRef =
    actorSystem.actorOf(
      Router.props(fch.hash(inputSeed+"remote"),Seq(networkControllerRef,peerManagerRef)),
      "Remote"
    )
  val localRef:ActorRef =
    actorSystem.actorOf(
    Router.props(fch.hash(inputSeed+"local"),Seq(networkControllerRef,peerManagerRef)),
    "Local"
  )
  val coordinatorRef:ActorRef =
    actorSystem.actorOf(
    Coordinator.props(fch.hash(inputSeed),Seq(routerRef,localRef)),
      "Coordinator"
    )
  coordinatorRef ! NewDataFile
  coordinatorRef ! Populate
  coordinatorRef ! Register
  coordinatorRef ! Run
  window match {
    case None =>
      Try{config.getString("params.password")}.toOption match {
        case Some(pwd) =>
          val keyFileDir = Try{config.getString("params.keyFileDir")}.toOption
          val dataDir = Try{config.getString("params.dataFileDir")}.toOption
          val yourName = Try{config.getString("params.yourName")}.toOption
          def getMostRecentDir(dir: String):Option[File] = {
            val d = new File(dir)
            if (d.exists && d.isDirectory) {
              Try{d.listFiles.filter(_.isDirectory).sortBy(f => f.lastModified()).toList.last} match {
                case Success(r) => Some(r)
                case Failure(e) =>
                  println("Error: could not find directory")
                  e.printStackTrace()
                  None
              }
            } else {
              None
            }
          }
          val dataLocation:Option[String] = Try{
            getMostRecentDir(dataDir.get).get.getPath
          }.toOption
          val keyLocation:Option[String] = Try{
            getMostRecentDir(keyFileDir.get).get.getPath
          }.toOption
          val agentName:Option[String] = Try{
            yourName.get + "_" + prosomoNodeUID.take(8)
          }.toOption
          val keyFile = Try {
            val kf:KeyFile = KeyFile.restore(keyLocation.get).get
            val keys = kf.getKeys(pwd, new Serializer, new Sig, new Vrf, new Kes)
            kf
          } match {
            case Success(kf) => Some(kf)
            case Failure(e) =>
              println("Error: could not load key")
              e.printStackTrace()
              None
          }
          Try{
            coordinatorRef ! NewHolderFromUI(
              keyFile.get,
              dataLocation.get,
              pwd,
              agentName.get,
              keyLocation.get
            )
          } match {
            case Success(_) =>
            case Failure(e) =>
              println("Error: could not load node information from given user input")
              e.printStackTrace()
          }
        case None =>
      }
    case Some(_) => if (!upnpFailed) Try{
      System.setOut(SharedData.printStream)
      window.get.declaredAddressField.get.peer.setOpaque(false)
      window.get.declaredAddressField.get.text = externalSocketAddress.get.toString
      window.get.connectButton.get.text = "Connected"
      window.get.window.get.reactions += {
        case event.WindowClosed(_) =>
          this.stopAll()
          window.get.runApp = false
          runApp = false
          System.setOut(SharedData.oldOut)
      }
      window.get.cmdButton.get.reactions += {
        case event.ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _)
          if window.get.cmdButton.get.hasFocus && window.get.cmdButton.get.enabled =>
          coordinatorRef ! GuiCommand(window.get.cmdField.get.text)
      }
      window.get.sendTxButton.get.reactions += {
        case scala.swing.event.ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _)
          if window.get.sendTxButton.get.hasFocus && window.get.sendTxButton.get.enabled =>
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
          window.get.txWin.get.confirmSendToNetworkWindow.get.close()
          window.get.txWin.get.issueTxWindow.get.close()
          window.get.txWin = None
          window.get.issueTxButton.get.enabled = true
      }
      window.get.coordRef = coordinatorRef
    } else {
      window.get.showUpnpWarn
      window.get.outputText.get.text = "Connection Failed"
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
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        stopAll()
      }
    })
  }

  def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    upnpGateway.foreach(_.deletePort(newPort))
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
    * input args will be either HOCON *.conf files in execution directory
    * or HOCON formatted strings that add to base config
    */
  val input:Array[String] = args
  val numAverageLoad = Parameters.numAverageLoad
  var loadAverage:Array[Double] = Array.fill(numAverageLoad){0.0}
  val sysLoad:SystemLoadMonitor = new SystemLoadMonitor
  val performanceFlag = Parameters.performanceFlag
  val useGui = Parameters.useGui
  val systemLoadThreshold = Parameters.systemLoadThreshold
  val config = Parameters.config

  

  def limiter:Unit = {
    if (performanceFlag) {
      val newLoad = sysLoad.cpuLoad
      if (newLoad>0.0){
        loadAverage = loadAverage.tail++Array(newLoad)
      }
      val cpuLoad = (0.0 /: loadAverage){_ + _}/loadAverage.length
      if (cpuLoad >= systemLoadThreshold) {
        SharedData.throwLimiterWarning("Start")
      } else if (cpuLoad < systemLoadThreshold) {
        SharedData.throwLimiterWarning("Stop")
      }
    }
  }

  var instance:Option[Prosomo] = None
  if (useGui) {
    val newWindow = Try{new ProsomoWindow(config)}.toOption
    //shared reference to window so stakeholder can enable buttons when started
    SharedData.prosomoWindow = newWindow
    newWindow match {
      case None =>
        instance = Try{new Prosomo(config,None)}.toOption
        Try{
          instance.get.run()
          while (instance.get.runApp) {
            Thread.sleep(100)
            limiter
          }
          instance.get.stopAll()
        }
      case Some(_) if newWindow.get.runApp =>
        instance = Try{new Prosomo(newWindow.get.windowConfig,Some(newWindow.get))}.toOption
        Try{
          instance.get.run()
          var i = 0
          while (newWindow.get.runApp) {
            newWindow.get.refreshOutput
            i+=1
            if (i%100==0) {i=0;newWindow.get.refreshPeerList;newWindow.get.refreshWallet()}
            if (i%10==0) limiter
            Thread.sleep(10)
          }
          instance.get.stopAll()
        } orElse Try{println("Error: instance terminated")}
      case _ =>
    }
  } else {
    instance = Try{new Prosomo(config,None)}.toOption
    Try{
      instance.get.run()
      while (instance.get.runApp) {
        Thread.sleep(100)
        limiter
      }
      instance.get.stopAll()
    }
  }
  Thread.sleep(100)
  System.exit(0)
}