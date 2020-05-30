package prosomo

import java.awt.{Color, Dimension}
import prosomo.primitives.Parameters.devMode
import prosomo.primitives.{ColorTextArea, SharedData}
import com.typesafe.config.{Config, ConfigFactory}
import javax.swing.{BorderFactory, SwingUtilities}
import scala.swing.Font.Style
import scala.swing._
import scala.util.Try
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

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

  def peerSeq:Seq[String] = {
    var out:Seq[String] = Seq()
    for (entry<-SharedData.guiPeerInfo) {
      out ++= Seq(entry._1)
      out ++= entry._2.map("  "+_.actorPath.toString)
    }
    out
  }

  var peerList = Try{
    new ListView(peerSeq) {
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
      maximumSize = new Dimension(150,50)
      minimumSize = new Dimension(150,50)
    }
  }.toOption

  val declaredAddressField = Try{
    new TextField {
      text = Try{windowConfig.getString("scorex.network.declaredAddress")}.toOption match {
        case Some(adr) if adr != "" => adr
        case None => prosomo.primitives.Parameters.declaredAddressFromRemote+":9084"
      }
      columns = 20
      editable = true
      maximumSize = new Dimension(150,50)
      minimumSize = new Dimension(150,50)
    }
  }.toOption

  val upnpCheck = Try{
    new CheckBox() {
      text = "UPNP"
    }
  }.toOption

  upnpCheck.get.reactions += {
    case event.ButtonClicked(_) => if (upnpCheck.get.selected) {declaredAddressField.get.enabled = false} else {declaredAddressField.get.enabled = true}
  }

  val connectButton = Try{
    new Button("Connect") {
      reactions += {
        case event.ButtonClicked(_) =>
          val useUpnp = if (upnpCheck.get.selected) {"yes"} else {"no"}
          val knownPeer = "\""+knownAddressField.get.text+"\""
          val declaredAddress = "\""+declaredAddressField.get.text+"\""
          val str = s"input{scorex{network{upnpEnabled=$useUpnp,knownPeers=[$knownPeer]}}}"
          windowConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(windowConfig)
          if (!upnpCheck.get.selected) Try{windowConfig.getString("scorex.network.declaredAddress")}.toOption match {
            case Some(adr) if adr != "" =>
            case _ => {
              val str = s"input{scorex{network{declaredAddress=$declaredAddress}}}"
              Try{
                windowConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(windowConfig)
              }.toOption match {
                case None => println("Error: input not parsed")
                case _ =>
              }
            }
          }
          Try{windowConfig.getString("scorex.network.agentName")}.toOption match {
            case Some(adr) if adr != "" =>
            case _ => {
              val str = "input{scorex{network{agentName=\"prosomo_"+prosomo.primitives.Parameters.prosomoNodeUID+"\"}}}"
              Try{
                windowConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(windowConfig)
              }.toOption match {
                case None => println("Error: input not parsed")
                case _ =>
              }
            }
          }
          upnpCheck.get.enabled = false
          waitToConnect = false
      }
    }
  }.toOption

  val connectElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new TextField("Bootstrap address: ") {
          editable=false
          maximumSize = new Dimension(122,50)
          minimumSize = new Dimension(122,50)
          border=BorderFactory.createEmptyBorder()
        }
        contents += knownAddressField.get
        contents += new TextField("  Declared address: ") {
          editable=false
          maximumSize = new Dimension(125,50)
          minimumSize = new Dimension(125,50)
          border=BorderFactory.createEmptyBorder()
        }
        contents += declaredAddressField.get
        maximumSize = new Dimension(2000,50)
        border=BorderFactory.createEmptyBorder()
      }
      contents += upnpCheck.get
      contents += connectButton.get
      maximumSize = new Dimension(2000,50)
      border=BorderFactory.createEmptyBorder()
    }
  }.toOption

  val issueTxButton = Try{
    new Button ("Issue Transaction") {
      enabled = false
    }
  }.toOption

  val recipientField = Try{
    new TextField {
      text = ""
      columns = 20
      editable = true
      maximumSize = new Dimension(150,50)
      minimumSize = new Dimension(150,50)
    }
  }.toOption

  val issueTxWindow:Option[Frame] = Try{
    new Frame {
      reactions += {
        case event.WindowClosing(_) => {

        }
      }
      title = "Issue Transaction"
      iconImage = toolkit.getImage("src/main/resources/Logo.png")
      contents = new BoxPanel(Orientation.Vertical) {
        border = Swing.EmptyBorder(10, 10, 10, 10)
        contents += recipientField.get
      }
      maximumSize = new Dimension(400,200)
      minimumSize = new Dimension(400,200)
      pack()
      centerOnScreen()
    }
  }.toOption

  val pendingTxField = Try{
    new TextField {
      editable=false
      border=BorderFactory.createEmptyBorder()
      text = {
        val (ptxs,ttxs,cb,pb) = SharedData.walletInfo
        s"Pending Txs: $ptxs" + s"   Confirmed Txs: $ttxs" + s"   Balance: $cb" + s"   Pending: $pb"
      }
      enabled = false
    }
  }.toOption

  val walletStats = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += pendingTxField.get
      maximumSize = new Dimension(2000,50)
      border=BorderFactory.createEmptyBorder()
    }
  }.toOption

  val walletElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += walletStats.get
      contents += issueTxButton.get
      maximumSize = new Dimension(2000,50)
      border=BorderFactory.createEmptyBorder()
    }
  }.toOption

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

  def refreshOutput = {
    val backgroundOperation: Future[Unit] = Future {
      val toAdd = SharedData.outText.toString
      SharedData.outText.reset()
      outputText.get.appendANSI(toAdd)
    }
    backgroundOperation onComplete { _ =>
      Swing.onEDT {
        if (!outputElem.get.verticalScrollBar.valueIsAdjusting) {
          while (outputText.get.lineCount > 2000) {
            outputText.get.text = outputText.get.text.drop(outputText.get.text.indexOf('\n')+1)
          }
          outputElem.get.verticalScrollBar.value = outputElem.get.verticalScrollBar.maximum
        }
      }
    }
  }

  def refreshPeerList = {
    SwingUtilities.invokeAndWait(()=>peerList.get.peer.setListData(peerSeq.toArray))
  }

  def refreshWallet = {
    if (pendingTxField.get.enabled) {
      val (ptxs,ttxs,cb,pb) = SharedData.walletInfo
      Swing.onEDT{
        pendingTxField.get.text = s"Pending Txs: $ptxs" + s"   Confirmed Txs: $ttxs" + s"   Balance: $cb" + s"   Pending: $pb"
      }
    }
  }

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
        contents += walletElem.get
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