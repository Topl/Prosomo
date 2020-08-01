package prosomo

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, Dimension}
import java.io.File

import akka.actor.ActorRef
import com.typesafe.config.{Config, ConfigFactory}
import javax.swing.{BorderFactory, Box, JOptionPane, SwingUtilities}
import prosomo.cases.NewHolderFromUI
import prosomo.components.Serializer
import prosomo.primitives.Parameters.{devMode, fch}
import prosomo.primitives._
import scorex.util.encode.Base58

import scala.swing.Font.Style
import scala.swing._
import scala.swing.event.{ButtonClicked, InputEvent, KeyReleased}
import scala.util.{Failure, Success, Try}
import com.formdev.flatlaf
import javax.swing.UIManager
import javax.swing.plaf.ColorUIResource
import com.formdev.flatlaf.util.DerivedColor

/**
  * AMS 2020:
  * A class for the user interface, will fail gracefully if any component is not able to load
  * This represents a proof of concept demo of all the elements needed to interact with the protocol
  * @param config base config to be modified by window elements
  */

/*

Outline:
  * Connection status and config
  * Basic wallet stats, transaction issue dialog box
 GUI elements in 4 tabs:
    * Active peers on network
    * Network stats, number of peers, active stake, tine qualities, block time
    * Node database management
    * Secure key creation, key management
  * Terminal output, max 2000 lines

 */

class ProsomoWindow(config:Config) extends ActionListener {

  val termColor:Color =    Color.getHSBColor(1.0.toFloat,0.0.toFloat,0.15.toFloat)
  val mainColor1:Color =   new Color(0, 118, 108)
  val mainColor2:Color =   new Color(38, 166, 154)
  val mainColor3:Color =   new Color(64, 93, 123)
  val accentColor1:Color = new Color(12, 41, 72)
  val accentColor2:Color = new Color(173, 209, 214)
  val accentColor3:Color = new Color(215, 233, 235)
  val baseColor1:Color =   new Color(68, 68, 68)
  val baseColor2:Color =   new Color(244, 248, 252)

  val peerDiscoveryAddress = "34.72.8.173:9084"
  val logo = Try{new javax.swing.ImageIcon(getClass.getClassLoader.getResource("Logo.png"))}.toOption
  val icon = Try{new javax.swing.ImageIcon(getClass.getClassLoader.getResource("Icon.png"))}.toOption

  Try{
    System.setProperty("apple.laf.useScreenMenuBar", "true")
    System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Prosomo")
    flatlaf.FlatDarkLaf.install()
    UIManager.setLookAndFeel(new flatlaf.FlatDarkLaf())
    val uidef = UIManager.getLookAndFeelDefaults()
//    uidef.entrySet().forEach(p => p.getValue match {
//      case c:Color => System.err.println(p.getKey.toString+": "+p.getKey.getClass.toString+" = "+c.toString)
//      case _ =>
//    })
    uidef.put("TabbedPane.focusColor", new ColorUIResource(mainColor1))
    uidef.put("TabbedPane.underlineColor", new ColorUIResource(mainColor2))
    uidef.put("Component.focusWidth",0)
    uidef.put("Component.innerFocusWidth",1)
    uidef.put("Button.arc",10)
    uidef.put("Button.focusedBorderColor",new ColorUIResource(mainColor1))
    uidef.put("Button.default.hoverBorderColor",new ColorUIResource(mainColor2))
    uidef.put("Button.hoverBorderColor",new ColorUIResource(mainColor2))
    uidef.put("CheckBox.icon.selectedFocusedBorderColor",new ColorUIResource(mainColor1))
    uidef.put("Button.default.focusedBorderColor",new ColorUIResource(mainColor1))
    uidef.put("Component.focusColor",new ColorUIResource(mainColor1))
    uidef.put("Button.default.focusColor",new ColorUIResource(mainColor1))
    uidef.put("CheckBox.icon.focusedBorderColor",new ColorUIResource(mainColor1))
    uidef.put("Component.focusedBorderColor",new ColorUIResource(mainColor3))
    flatlaf.FlatLaf.initIconColors(uidef, true )
  } match {
    case Success(_) =>
    case Failure(e) => e.printStackTrace()
  }

  var windowConfig:Config = config
  var waitToConnect = true
  var runApp = true
  val listener = this

  var coordRef:ActorRef = _

  def uuid: String = java.util.UUID.randomUUID.toString

  def getMostRecentDir(dir: String):Option[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      Try{d.listFiles.filter(_.isDirectory).sortBy(f => f.lastModified()).toList.last} match {
        case Success(r) => Some(r)
        case Failure(_) => None
      }
    } else {
      None
    }
  }

  def bytes2hex(b: Array[Byte]): String = {
    b.map("%02x" format _).mkString
  }

  def hex2bytes(hex: String): Array[Byte] = {
    if (hex.contains(" ")) {
      hex.split(" ").map(Integer.parseInt(_, 16).toByte)
    } else if (hex.contains("-")) {
      hex.split("-").map(Integer.parseInt(_, 16).toByte)
    } else {
      hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    }
  }

  /********************************************************************************************************************/

  val cmdField = Try{
    new TextField {
      text = ""
      columns = 20
      editable = true
      maximumSize = new Dimension(2000,50)
    }
  }.toOption

  val cmdButton = Try{
    new Button("Issue Command")
  }.toOption

  val commandElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += cmdField.get
      contents += cmdButton.get
      border = Swing.EmptyBorder(0, 0, 10, 0)
      focusable = false
    }
  }.toOption

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
      focusable = false
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

    }
  }.toOption

  val peerListElem = Try{
    new ScrollPane(peerList.get) {
      maximumSize = new Dimension(2000,2000)
      preferredSize = new Dimension(800,400)
      minimumSize = new Dimension(100,100)
      focusable = false
    }
  }.toOption

  val knownAddressField = Try{
    new TextField {
      text = peerDiscoveryAddress
      columns = 20
      editable = true
      maximumSize = new Dimension(150,50)
      minimumSize = new Dimension(150,50)
    }
  }.toOption

  val uiDeclaredAddress = Try{windowConfig.getString("scorex.network.declaredAddress")}.toOption match {
    case Some(adr) if adr != "" => adr
    case None => {
      prosomo.primitives.Parameters.declaredAddressFromRemote match {
        case Some(str) =>str+":9084"
        case None => ""
      }
    }
  }

  val declaredAddressField = Try{
    new TextField {
      columns = 20
      editable = true
      maximumSize = new Dimension(150,50)
      minimumSize = new Dimension(150,50)
      enabled = false
      text = ""
      peer.setOpaque(false)
    }
  }.toOption

  val upnpCheck = Try{
    new CheckBox() {
      text = "UPNP"
      selected = true
      border = Swing.EmptyBorder(0, 10, 0, 10)
      listenTo(keys)
    }
  }.toOption

  Try{
    upnpCheck.get.reactions += {
      case event.ButtonClicked(_) => if (upnpCheck.get.selected) {
        declaredAddressField.get.enabled = false
        declaredAddressField.get.focusable = false
        declaredAddressField.get.text = ""
        declaredAddressField.get.peer.setOpaque(false)
      } else {
        declaredAddressField.get.enabled = true
        declaredAddressField.get.focusable = true
        declaredAddressField.get.text = uiDeclaredAddress
        declaredAddressField.get.peer.setOpaque(true)
      }
    }
    upnpCheck.get.reactions += {
      case event.KeyPressed(_, event.Key.Enter, _, _) if upnpCheck.get.hasFocus =>
        if (upnpCheck.get.selected) {
          upnpCheck.get.selected = false
          declaredAddressField.get.enabled = true
          declaredAddressField.get.focusable = true
          declaredAddressField.get.text = uiDeclaredAddress
          declaredAddressField.get.peer.setOpaque(true)
        } else {
          upnpCheck.get.selected = true
          declaredAddressField.get.enabled = false
          declaredAddressField.get.focusable = false
          declaredAddressField.get.text = ""
          declaredAddressField.get.peer.setOpaque(false)
        }
    }
  }

  val nameField = Try{
    new TextField {
      text = ""
      editable = true
      maximumSize = new Dimension(250,30)
      minimumSize = new Dimension(250,30)
      listenTo(keys)
      reactions += {
        case event: KeyReleased => listener.actionPerformed(new ActionEvent(this,0,"0"))
        case _ =>
      }
      peer.addActionListener(listener)
    }
  }.toOption


  val connectButton = Try{
    new Button("Connect") {
      enabled = false
      tooltip = "Enter a name below before you connect"
      listenTo(keys)
      listenTo(nameField.get.keys)
      reactions += {
        case event.ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus || nameField.get.hasFocus && enabled => {
          val useUpnp = if (upnpCheck.get.selected) {"yes"} else {"no"}
          val str1 = s"input{scorex{network{upnpEnabled=$useUpnp}}}"
          windowConfig = ConfigFactory.parseString(str1).getConfig("input").withFallback(windowConfig)

          val knownPeer = knownAddressField.get.text
          val str2 = "input{scorex{network{knownPeers=[\""+knownPeer+"\"]}}}"
          if (knownPeer != "") windowConfig = ConfigFactory.parseString(str2).getConfig("input").withFallback(windowConfig)

          val declaredAddress = declaredAddressField.get.text
          if (!upnpCheck.get.selected) Try{config.getString("scorex.network.declaredAddress")}.toOption match {
            case Some(adr) if adr != "" =>
            case _ => if (declaredAddress != "") {
              val str = "input{scorex{network{declaredAddress=\""+declaredAddress+"\"}}}"
              Try{
                windowConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(windowConfig)
              }.toOption match {
                case None => println("Error: input not parsed")
                case _ =>
              }
            }
          }

          Try{windowConfig.getString("scorex.network.agentName")}.toOption match {
            case Some(adr) if adr != "" && adr != "prosomo" =>
            case _ => {
              val str = "input{scorex{network{agentName=\""+agentNameField.get.text+"\"}}}"
              Try{
                windowConfig = ConfigFactory.parseString(str).getConfig("input").withFallback(windowConfig)
              }.toOption match {
                case None => println("Error: input not parsed")
                case _ =>
              }
            }
          }

          Try{windowConfig.getString("scorex.network.nodeName")}.toOption match {
            case Some(adr) if adr != "" && adr != "prosomo" =>
            case _ => {
              val str = "input{scorex{network{nodeName=\""+nameField.get.text+"\"}}}"
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
          activePane.get.pages(1).enabled = true
          activePane.get.pages(2).enabled = true
          activePane.get.pages(3).enabled = true
          activePane.get.peer.setSelectedIndex(3)
          Try{getMostRecentDir(dataDir.getPath)} match {
            case Success(folderOpt) => folderOpt match {
              case Some(folder) =>
                nodeViewFileChooser.get.peer.setSelectedFile(folder)
                openNodeViewButton.get.enabled = true
              case None =>
            }
            case _ =>
          }
          window.get.peer.revalidate()
          activePane.get.requestFocusInWindow()
        }
      }
    }
  }.toOption

  val connectElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new TextField("Bootstrap address: ") {
          editable = false
          maximumSize = new Dimension(143,50)
          minimumSize = new Dimension(143,50)
          border = BorderFactory.createEmptyBorder()
          peer.setOpaque(false)
          border = Swing.EmptyBorder(0, 11, 0, 0)
        }
        contents += knownAddressField.get
        contents += new TextField("  Declared address: ") {
          editable = false
          maximumSize = new Dimension(135,50)
          minimumSize = new Dimension(135,50)
          border=BorderFactory.createEmptyBorder()
          peer.setOpaque(false)
          focusable = false
        }
        contents += declaredAddressField.get
        maximumSize = new Dimension(2000,50)
        border = BorderFactory.createEmptyBorder()
      }
      contents += upnpCheck.get
      contents += connectButton.get
      maximumSize = new Dimension(2000,50)
      border = Swing.EmptyBorder(0, 0, 10, 10)
      focusable = false
    }
  }.toOption

  /********************************************************************************************************************/

  var txWin:Option[IssueTxWindow] = None

  val issueTxButton = Try{
    new Button ("Issue Transaction") {
      enabled = false
      listenTo(keys)
      reactions += {
        case scala.swing.event.ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled  => {
          enabled = false
          txWin match {
            case None =>
            case Some(_) => txWin = None
          }
          txWin = Try{
            new IssueTxWindow(
              SharedData.issueTxInfo match {
                case Some(info) => Base58.encode(info._1.data)
                case None => ""
              },
              SharedData.issueTxInfo.get._2.toSeq.map {
              info =>
                val pks = info._2._2
                val pkw: String = Base58.encode(pks._1 ++ pks._2 ++ pks._3)
                info._2._1.actorPath.toString() + " " + pkw
              }
            )
          } match {
            case Success(v) => Some(v)
            case Failure(e) =>
              e.printStackTrace()
              None
          }
          txWin.get.issueTxWindow.get.reactions += {
            case event.WindowClosing(_) => {
              txWin = None
            }
          }
          txWin.get.issueTxWindow.get.open()
        }
      }
    }
  }.toOption

  val sendToNetworkButton = Try{
    new Button ("Send To Network") {
      listenTo(keys)
      reactions += {
        case scala.swing.event.ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled =>
          confirmSendToNetworkWindow.get.open()
      }
    }
  }.toOption

  val sendTxButton = Try {
    new Button ("Send") {listenTo(keys)}
  }.toOption

  val confirmSendToNetworkWindow = Try {
    new Frame {
      title = "Confirm"
      iconImage = icon.get.getImage
      contents = new BorderPanel {
        border = Swing.EmptyBorder(10, 10, 10, 10)
        add(sendTxButton.get,BorderPanel.Position.East)
        add(new Button ("Cancel") {
          listenTo(keys)
          reactions += {
            case scala.swing.event.ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled => {
              close()
            }
          }
        },BorderPanel.Position.West)
      }
      maximumSize = new Dimension(240,50)
      minimumSize = new Dimension(240,50)
      pack()
      centerOnScreen()
    }
  }.toOption

  class IssueTxWindow(sender:String,inbox:Seq[String]) extends ActionListener {

    val fieldWidth = 600
    val fieldHight = 30
    val dummyKey = ' '.toString * 100
    val adr = inbox.map(str=>str.split(" ")(0))
    val listkeys = {
      var out:Map[String,String] = Map(dummyKey->"")
      inbox.foreach(str=>out += (str.split(" ")(0)->str.split(" ")(1)))
      out
    }

    val deltaField = Try{
      new TextField {
        columns = 92
        editable = true
        minimumSize = new Dimension(fieldWidth,fieldHight)
        maximumSize = new Dimension(fieldWidth,fieldHight)
        horizontalAlignment = Alignment.Right
      }
    }.toOption

    val txDeltaElem = Try{
      new BoxPanel(Orientation.Horizontal) {
        contents += new TextField("  Amount To Send: ") {
          editable = false
          border = BorderFactory.createEmptyBorder()
          focusable = false
        }
        contents += deltaField.get
        peer.setOpaque(false)
        border = Swing.EmptyBorder(10, 0, 10, 0)
      }
    }.toOption

    val recipField = Try{
      new TextField {
        columns = 92
        editable = true
        minimumSize = new Dimension(fieldWidth,fieldHight)
        maximumSize = new Dimension(fieldWidth,fieldHight)
        horizontalAlignment = Alignment.Right
      }
    }.toOption

    val recipDropList = Try{
      new ComboBox[String](Seq(dummyKey)++adr) {
        minimumSize = new Dimension(fieldWidth,fieldHight)
        maximumSize = new Dimension(fieldWidth,fieldHight)
      }
    }.toOption

    recipDropList.get.peer.addActionListener(this)

    val issueTxWindow:Option[Frame] = Try{
      new Frame {
        val recipElem = Try{
          new BoxPanel(Orientation.Horizontal) {
            contents += new TextField("  Recipient Public Address: ") {
              editable = false
              border = BorderFactory.createEmptyBorder()
              peer.setOpaque(false)
              focusable = false
            }
            contents += recipField.get
            border = Swing.EmptyBorder(10, 0, 10, 0)
          }
        }.toOption

        val senderField = Try{
          new TextField {
            text = sender
            columns = 92
            editable = false
            minimumSize = new Dimension(fieldWidth,fieldHight)
            maximumSize = new Dimension(fieldWidth,fieldHight)
            horizontalAlignment = Alignment.Right
            peer.setOpaque(false)
          }
        }.toOption

        val senderElem = Try{
          new BoxPanel(Orientation.Horizontal) {
            contents += new TextField("  Sender Public Address: ") {
              editable=false
              border = Swing.EmptyBorder(10, 0, 10, 0)
              peer.setOpaque(false)
            }
            contents += senderField.get
          }
        }.toOption

        title = "Issue Transaction"
        iconImage = icon.get.getImage
        contents = new BoxPanel(Orientation.Vertical) {
          border = Swing.EmptyBorder(10, 10, 10, 10)
          contents += senderElem.get
          contents += {
            new BoxPanel(Orientation.Horizontal) {
              contents += new TextField("  Choose Public Address: ") {
                editable = false
                border = BorderFactory.createEmptyBorder()
                peer.setOpaque(false)
                focusable = false
              }
              contents += recipDropList.get
              border = Swing.EmptyBorder(10, 0, 10, 0)
            }
          }
          contents += recipElem.get
          contents += txDeltaElem.get
          contents += sendToNetworkButton.get
        }

        maximumSize = new Dimension(1200,200)
        minimumSize = new Dimension(1200,200)
        pack()
        centerOnScreen()
        override def closeOperation() = {
          issueTxButton.get.enabled = true
        }
      }
    } match {
      case Success(v) => Some(v)
      case Failure(e) =>
        e.printStackTrace()
        None
    }

    override def actionPerformed(e: ActionEvent): Unit = {
      e.getSource match {
        case value if value == recipDropList.get.peer =>  recipField.get.text = listkeys(recipDropList.get.selection.item)
        case _ =>
      }
    }
  }

  val pendingTxField = Try{
    new TextField {
      editable=false
      border=BorderFactory.createEmptyBorder()
      text = {
        val (ptxs,ttxs,cb,pb) = SharedData.walletInfo
        s"Pending Txs: $ptxs" + s"   Confirmed Txs: $ttxs" + s"   Balance: $cb" + s"   Pending: $pb"
      }
      enabled = false
      peer.setOpaque(false)
      focusable = false
      border = Swing.EmptyBorder(0, 11, 0, 0)
    }
  }.toOption

  val walletStats = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += pendingTxField.get
      maximumSize = new Dimension(2000,50)
      border=BorderFactory.createEmptyBorder()
      focusable = false
    }
  }.toOption

  val walletElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += walletStats.get
      contents += issueTxButton.get
      maximumSize = new Dimension(2000,50)
      border = Swing.EmptyBorder(10, 0, 10, 10)
      focusable = false
    }
  }.toOption

  val outputText = Try{
    new ColorTextArea {
      editable = false
      font = swing.Font("Monospaced",Style.Plain,14)
      background = termColor
      foreground = mainColor2
      lineWrap = true
      focusable = false
    }
  }.toOption

  val outputElem = Try{
    new ScrollPane(outputText.get) {
      verticalScrollBar.value = verticalScrollBar.maximum
      maximumSize = new Dimension(2000,2000)
      preferredSize = new Dimension(800,400)
      minimumSize = new Dimension(100,100)
      focusable = false
    }
  }.toOption

  /********************************************************************************************************************/

  Try{
    javax.swing.UIManager.put("FileChooser.readOnly", true)
  }

  val keyLocation = Try{config.getString("params.keyFileDir")}.toOption

  val keyDir = new File(keyLocation.get)

  Try{keyDir.mkdirs()}

  val keysFileChooser = Try {
    new FileChooser(keyDir) {
      fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      listenTo(mouse.clicks)
      listenTo(mouse.moves)
      listenTo(mouse.wheel)
      listenTo(keys)
      reactions += {
        case _:InputEvent | event.KeyPressed(_,_,_,_) => listener.actionPerformed(new ActionEvent(this,2,"2"))
        case _ =>
      }
      peer.setControlButtonsAreShown(false)
    }
  }.toOption

  var keyFileDir = ""

  val newKeyButton = Try{
    new Button ("Create New Key") {
      listenTo(keys)
      reactions += {
        case ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled => {
          keysFileChooser.get.peer.rescanCurrentDirectory()
          keyWin match {
            case None =>
            case Some(_) => keyWin = None
          }
          keyWin = Try{new KeyManagerWindow(true)}.toOption
          keyWin.get.window.get.open()
        }
      }
    }
  }.toOption

  val loadKeyButton = Try{
    new Button ("Load Key") {
      enabled = false
      listenTo(keys)
      reactions += {
        case ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled =>
          keyWin match {
            case None =>
            case Some(_) => keyWin = None
          }
          keyWin = Try{new KeyManagerWindow(false)}.toOption
          keyWin.get.window.get.open()
      }
    }
  }.toOption

  var keyWin:Option[KeyManagerWindow] = None

  class KeyManagerWindow(val newKey:Boolean) extends ActionListener {

    var windowEntropy1 = fch.hash(uuid)
    var windowEntropy2 = fch.hash(uuid)
    var windowEntropy3 = fch.hash(uuid)

    var windowKeyFile = KeyFile.empty

    val passwordHelp = Try {
      new TextField(if(newKey){"  Enter a new password, 6 characters or longer:  "}else{"  Enter Password:  "}) {
        editable = false
        border = BorderFactory.createEmptyBorder()
        peer.setOpaque(false)
        focusable = false
      }
    }.toOption

    val passwordField = Try{
      new PasswordField() {
        text = ""
        columns = 30
        listenTo(keys)
        reactions += {
          case event: KeyReleased => listener.actionPerformed(new ActionEvent(this,5,"5"))
          case _ =>
        }
        maximumSize = new Dimension(125,50)
        minimumSize = new Dimension(125,50)
      }
    }.toOption

    val confirmHelp = Try {
      new TextField("  Confirm Password:  ") {
        editable = false
        border = BorderFactory.createEmptyBorder()
        peer.setOpaque(false)
        focusable = false
      }
    }.toOption

    val confirmPasswordField = Try{
      new PasswordField() {
        text = ""
        columns = 30
        listenTo(keys)
        reactions += {
          case event: KeyReleased => listener.actionPerformed(new ActionEvent(this,5,"5"))
          case _ =>
        }
        maximumSize = new Dimension(125,50)
        minimumSize = new Dimension(125,50)
      }
    }.toOption

    val bip39Help = Try {
      new TextField("  Enter or generate BIP39 mnemonic phrase: ") {
        editable = false
        border = BorderFactory.createEmptyBorder()
        peer.setOpaque(false)
        focusable = false
      }
    }.toOption

    val bip39Field = Try{
      new TextField("") {
        columns = 120
      }
    }

    val bip39 = Bip39("en")

    val readyButton = Try{
      new Button (if(newKey) "Generate" else "Load") {
        if (newKey) {
          enabled = false
          listenTo(keys)
          reactions += {
            case ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled => {
              val file = new File(keyLocation.get+"/"+agentNameField.get.text+"_"+System.currentTimeMillis().toString+"/")
              file.mkdirs()
              keysFileChooser.get.selectedFile = file
              keysFileChooser.get.peer.rescanCurrentDirectory()
            }
          }
        } else {
          listenTo(keys)
          listenTo(passwordField.get.keys)
        }
      }
    }.toOption

    val window = Try{
      new Frame {
        title = if (newKey) {"Create Key"} else {"Load Key"}
        iconImage = icon.get.getImage
        contents = new BoxPanel(Orientation.Vertical) {
          border = Swing.EmptyBorder(10, 10, 10, 10)
          contents += new BoxPanel(Orientation.Horizontal) {
            contents += passwordHelp.get
            contents += passwordField.get
            border = Swing.EmptyBorder(0, 0, 10, 0)
          }
          if (newKey) contents += new BoxPanel(Orientation.Horizontal) {
            contents += confirmHelp.get
            contents += confirmPasswordField.get
            border = Swing.EmptyBorder(0, 0, 10, 0)
          }
          if (newKey) contents += {
            new BoxPanel(Orientation.Horizontal) {
              contents += bip39Help.get
              contents += new Button ("Seed New Phrase") {
                listenTo(keys)
                reactions += {
                  case scala.swing.event.ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled =>
                    windowEntropy1 = fch.hash(uuid)
                    bip39Field.get.text = bip39.uuidSeedPhrase(bytes2hex(windowEntropy1))._2
                    windowEntropy2 = fch.hash(uuid)
                    windowEntropy3 = fch.hash(uuid)
                    bip39Field.get.editable = false
                    JOptionPane.showMessageDialog(
                      this.peer,
                      "A new mnemonic phrase has been generated for you.\nWrite it down, it will not be shown again.\nYou may use it to recover your account in the future.",
                      "New Phrase",
                      JOptionPane.INFORMATION_MESSAGE,
                      logo.get
                    )
                }
              }
              border = Swing.EmptyBorder(10, 0, 10, 0)
            }
          }
          if (newKey) contents += bip39Field.get
          contents += new BoxPanel(Orientation.Horizontal) {
            contents += readyButton.get
            peer.add(Box.createHorizontalStrut(10))
            contents += new Button ("Cancel") {
              listenTo(keys)
              reactions += {
                case scala.swing.event.ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled => {
                  close()
                }
              }
            }
            maximumSize = new Dimension(200,30)
            border = Swing.EmptyBorder(10, 10, 10, 10)
          }
        }
        if (newKey) {
          minimumSize = new Dimension(800,200)
        }
        readyButton.get.reactions += {
          case scala.swing.event.ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _)
            if readyButton.get.hasFocus && readyButton.get.enabled || passwordField.get.hasFocus =>
            keyFileDir = keysFileChooser.get.selectedFile.getPath
            if (newKey) {
              if (bip39.validateInputPhrase(bip39Field.get.text)) {
                Try {
                  val keyFile:KeyFile = KeyFile.fromSeed(
                    passwordField.get.peer.getPassword.mkString,keyFileDir,
                    new Serializer,
                    new Sig,
                    new Vrf,
                    new Kes,
                    SharedData.globalSlot,
                    fch.hash(hex2bytes(bip39.phraseToHex(bip39Field.get.text))),
                    windowEntropy2,
                    windowEntropy3
                  )
                  val keys = keyFile.getKeys(passwordField.get.peer.getPassword.mkString, new Serializer, new Sig, new Vrf, new Kes)
                  keyFile
                }.toOption match {
                  case None =>
                    JOptionPane.showMessageDialog(this.peer, "Key generation failed", "Error", JOptionPane.WARNING_MESSAGE,logo.get)
                  case Some(kf) =>
                    windowKeyFile = kf
                    listener.actionPerformed(new ActionEvent(this,4,"4"))
                }
              } else {
                JOptionPane.showMessageDialog(
                  this.peer,
                  "Invalid BIP39 mnemonic phrase.\nA valid phrase can have 12, 15, 18, 21, or 24 words.\nTry generating a new phrase.",
                  "Invalid Phrase",
                  JOptionPane.WARNING_MESSAGE,
                  logo.get
                )
              }
            } else {
              Try {
                val keyFile:KeyFile = KeyFile.restore(keyFileDir).get
                val keys = keyFile.getKeys(passwordField.get.peer.getPassword.mkString, new Serializer, new Sig, new Vrf, new Kes)
                keyFile
              }.toOption match {
                case None =>
                  JOptionPane.showMessageDialog(this.peer, "Password is incorrect.\nTry again.", "Invalid Password", JOptionPane.WARNING_MESSAGE,logo.get)
                case Some(kf) =>
                  windowKeyFile = kf
                  listener.actionPerformed(new ActionEvent(this,4,"4"))
              }
            }
        }
        pack()
        centerOnScreen()
      }
    }

    override def actionPerformed(e: ActionEvent): Unit = {
      e.getSource match {
        case _ =>
      }
    }
  }

  val keysFileElem = Try {
    new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new TextField("   Select the directory of your key, keep one account per directory, old keys are erased   "){
          editable = false
          border = BorderFactory.createEmptyBorder()
          peer.setOpaque(false)
          focusable = false
        }
        contents += loadKeyButton.get
        peer.add(Box.createHorizontalStrut(10))
        contents += newKeyButton.get
      }
      contents += keysFileChooser.get
      border = BorderFactory.createEmptyBorder()
      focusable = false
      border = Swing.EmptyBorder(10, 0, 0, 10)
    }
  }.toOption

  /********************************************************************************************************************/

  val dataDir = new File(config.getString("params.dataFileDir"))

  Try{dataDir.mkdirs()}

  var dataLocation = ""

  val nodeViewFileChooser = Try {
    new FileChooser(dataDir) {
      fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      peer.setControlButtonsAreShown(false)
      listenTo(mouse.clicks)
      listenTo(mouse.moves)
      listenTo(mouse.wheel)
      listenTo(keys)
      reactions += {
        case _: event.InputEvent | event.KeyPressed(_,_,_,_) | event.KeyReleased(_,_,_,_) => listener.actionPerformed(new ActionEvent(this,1,"1"))
        case _ =>
      }
    }
  }

  val newNodeViewButton = Try{
    new Button ("Create Database") {
      listenTo(keys)
      reactions += {
        case ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled => {
          val file = new File(dataDir+"/"+agentNameField.get.text+"_"+System.currentTimeMillis().toString+"/")
          file.mkdirs()
          nodeViewFileChooser.get.selectedFile = file
          nodeViewFileChooser.get.peer.rescanCurrentDirectory()
          dataLocation = nodeViewFileChooser.get.selectedFile.getPath
          listener.actionPerformed(new ActionEvent(this,3,"3"))
        }
      }
    }
  }.toOption

  val openNodeViewButton = Try {
    new Button ("Load Database") {
      enabled = false
      listenTo(keys)
      reactions += {
        case ButtonClicked(_) | event.KeyPressed(_, event.Key.Enter, _, _) if hasFocus && enabled => {
          dataLocation = nodeViewFileChooser.get.selectedFile.getPath
          listener.actionPerformed(new ActionEvent(this,3,"3"))
        }
      }
    }
  }.toOption

  val nodeViewFileElem = Try {
    new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new TextField("   Select the directory of your node data, including block and state databases   "){
          editable = false
          border = BorderFactory.createEmptyBorder()
          peer.setOpaque(false)
          focusable = false
        }
        contents += openNodeViewButton.get
        peer.add(Box.createHorizontalStrut(10))
        contents += newNodeViewButton.get
      }
      contents += nodeViewFileChooser.get
      border = BorderFactory.createEmptyBorder()
      focusable = false
      border = Swing.EmptyBorder(10, 0, 0, 10)
    }
  }.toOption

  /********************************************************************************************************************/

  val nameHelp = Try{
    new TextField {
      text = "Enter a name with no special characters, then press Connect: "
      editable = false
      maximumSize = new Dimension(500,30)
      minimumSize = new Dimension(500,30)
      border=BorderFactory.createEmptyBorder()
      peer.setOpaque(false)
      focusable = false
    }
  }.toOption

  val agentNameHelp = Try{
    new TextField {
      text = "Your agent name as seen by peers: "
      editable = false
      maximumSize = new Dimension(250,30)
      minimumSize = new Dimension(250,30)
      border=BorderFactory.createEmptyBorder()
      peer.setOpaque(false)
      focusable = false
    }
  }.toOption

  val agentNameField = Try{
    new TextField {
      text = ""
      columns = 100
      editable = false
      maximumSize = new Dimension(500,30)
      minimumSize = new Dimension(500,30)
      peer.setOpaque(false)
      focusable = false
    }
  }.toOption

  val netStats = Try{
    new TextArea {
      text = ""
      editable = false
      border=BorderFactory.createEmptyBorder()
      focusable = false
    }
  }.toOption

  val nameElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += nameHelp.get
      contents += nameField.get
      minimumSize = new Dimension(100,30)
      border = Swing.EmptyBorder(10, 10, 10, 10)
      focusable = false
    }
  }.toOption

  val agentNameElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += agentNameHelp.get
      contents += agentNameField.get
      minimumSize = new Dimension(100,30)
      border = Swing.EmptyBorder(10, 10, 10, 10)
      focusable = false
    }
  }.toOption

  val nameNetBoxElem = Try{
    new BoxPanel(Orientation.Vertical) {
      contents += nameElem.get
      contents += agentNameElem.get
      contents += netStats.get
      maximumSize = new Dimension(2000,2000)
      preferredSize = new Dimension(800,400)
      minimumSize = new Dimension(100,100)
      focusable = false
    }
  }.toOption

  val netStatsElem = Try{
    new ScrollPane(nameNetBoxElem.get) {
      focusable = false
    }
  }.toOption

  /********************************************************************************************************************/

  val stakePoolElem = Try {
    new ScrollPane(new TextArea("\n    Stake pool operations will be handled here in a future version."){
      editable = false
      focusable = false
    })
  }.toOption

  val activePane = Try{
    new TabbedPane {
      pages += new TabbedPane.Page("Network Info",netStatsElem.get)
      pages += new TabbedPane.Page("Active Peers",peerListElem.get)
      pages += new TabbedPane.Page("Stake Pool",stakePoolElem.get)
      pages += new TabbedPane.Page("Manage Database",nodeViewFileElem.get)
      pages += new TabbedPane.Page("Manage Keys",keysFileElem.get)
      tooltip = "Select a tab"
      maximumSize = new Dimension(2000,2000)
      preferredSize = new Dimension(800,400)
      minimumSize = new Dimension(100,100)
    }
  }.toOption

  Try {
    (1 to 4).foreach(activePane.get.pages(_).enabled = false)
  }

  Try {
    activePane.get.peer.setToolTipTextAt(0,"Network name and stats")
    activePane.get.peer.setToolTipTextAt(1,"List of discovered peers with stake info")
    activePane.get.peer.setToolTipTextAt(2,"View and manage your stake pool")
    activePane.get.peer.setToolTipTextAt(3,"Choose the blockchain data on disk")
    activePane.get.peer.setToolTipTextAt(4,"Create or load your keys from disk")
  }

  def refreshOutput = {
    Swing.onEDT {
      if (!outputElem.get.verticalScrollBar.valueIsAdjusting) {
        outputText.get.appendANSI(SharedData.outText.toString)
        SharedData.outText.reset()
        while (outputText.get.lineCount > 2000) {
          outputText.get.text = outputText.get.text.drop(outputText.get.text.indexOf('\n')+1)
        }
        outputElem.get.verticalScrollBar.value = outputElem.get.verticalScrollBar.maximum
      }
    }
  }

  def refreshPeerList = {
    SwingUtilities.invokeAndWait(()=>peerList.get.peer.setListData(peerSeq.toArray))
  }

  def refreshWallet() = {
    if (pendingTxField.get.enabled) {
      val (ptxs,ttxs,cb,pb) = SharedData.walletInfo
      Swing.onEDT{
        pendingTxField.get.text = s"Pending Txs: $ptxs" + s"   Confirmed Txs: $ttxs" + s"   Balance: $cb" + s"   Pending: $pb"
      }
    }
    Swing.onEDT{
      netStats.get.text = {
        var text = "\n\n"
        text += s"        Number of active peers discovered on the network: ${SharedData.activePeers}\n\n"
        text += f"        Proportion of active stake online in the current staking distribution: ${SharedData.activeStake}%1.5f\n\n"
        text += f"        Average block time as global slot divided by block number: ${SharedData.blockTime}%5.5f\n\n"
        text += f"        Proportion of active slots as block number divided by global slot: ${SharedData.activeSlots}%5.5f\n\n"
        text += f"        Average transactions per second over entire chain: ${SharedData.txsPerSecond}%5.5f\n\n"
        text += s"        Transactions in mempool: ${SharedData.numTxsMempool}\n\n"
        text += f"        Average block delay: ${SharedData.averageNetworkDelay}%5.5f\n\n"
        text
      }
    }
  }

  val windowContents = Try{
    new BoxPanel(Orientation.Vertical) {
      border = Swing.EmptyBorder(10, 10, 10, 10)
      if (devMode) {
        contents += commandElem.get
        contents += new Separator()
      }
      contents += connectElem.get
      contents += new Separator()
      contents += walletElem.get
      contents += new Separator()
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new SplitPane(Orientation.Horizontal,activePane.get,outputElem.get) {
          border = Swing.EmptyBorder(0, 0, 0, 0)
        }
        border = Swing.EmptyBorder(0, 0, 0, 0)
        focusable = false
      }
    }
  }.toOption

  val window:Option[Frame] = Try{
    new Frame {
      reactions += {
        case event.WindowClosing(_) =>
          waitToConnect = false
          runApp = false
          System.setOut(SharedData.oldOut)
          prosomo.primitives.Parameters.useGui = false
      }
      title = "Prosomo"
      iconImage = icon.get.getImage
      contents = windowContents.get
      pack()
      centerOnScreen()
      open()
      nameField.get.requestFocusInWindow()
    }
  }.toOption

  def showUpnpWarn = Try{
    JOptionPane.showMessageDialog(windowContents.get.peer, "A Upnp device could not be found.\nTry connecting with Upnp disabled.\nIf you have access to your router,\nyou may have to manually forward ports.\nRestart the application to try again.", "Connection Failed", JOptionPane.WARNING_MESSAGE,logo.get)
  }

  window match {
    case None => Try{
      prosomo.primitives.Parameters.useGui = false
    }
    case Some(frame) => Try{
      while (waitToConnect) {
        Thread.sleep(100)
      }
      if (runApp) {
        nameField.get.enabled = false
        nameNetBoxElem.get.contents -= nameElem.get
        nameNetBoxElem.get.contents -= agentNameElem.get
        connectButton.get.enabled = false
        connectButton.get.text = "Connecting..."
        knownAddressField.get.editable = false
        declaredAddressField.get.editable = false
        declaredAddressField.get.enabled = true
        declaredAddressField.get.focusable = true
        outputText.get.text = "Loading..."
      }
    }
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    e.getActionCommand match {
      case "0" =>
        agentNameField.get.text = nameField.get.text + "_" + prosomo.primitives.Parameters.prosomoNodeUID.take(8)
        if (nameField.get.text != ""
          && nameField.get.text.forall((('a'to'z')++('A'to'Z')++('0'to'9')).toSet.contains(_))
        ) {
          connectButton.get.enabled = true
        } else {connectButton.get.enabled = false}
      case "1" =>
        nodeViewFileChooser.get.peer.getSelectedFile match {
          case null =>
            openNodeViewButton.get.enabled = false
          case file if file.exists() =>
            openNodeViewButton.get.enabled = true
          case _ =>
            openNodeViewButton.get.enabled = false
        }
      case "2" =>
        keysFileChooser.get.peer.getSelectedFile match {
          case null => loadKeyButton.get.enabled = false
          case file if file.exists() => loadKeyButton.get.enabled = true
          case _ => loadKeyButton.get.enabled = false
        }
      case "3" =>
        activePane.get.pages(3).enabled = false
        activePane.get.pages(4).enabled = true
        activePane.get.peer.setSelectedIndex(4)
        Try{getMostRecentDir(keyDir.getPath)} match {
          case Success(folderOpt) => folderOpt match {
            case Some(folder) =>
              keysFileChooser.get.peer.setSelectedFile(folder)
              loadKeyButton.get.enabled = true
            case None =>
          }
          case _ =>
        }
        window.get.peer.revalidate()
        activePane.get.requestFocusInWindow()
      case "4" =>
        keyWin.get.window.get.close()
        activePane.get.pages(4).enabled = false
        activePane.get.peer.setSelectedIndex(0)
        coordRef ! NewHolderFromUI(keyWin.get.windowKeyFile,dataLocation,keyWin.get.passwordField.get.peer.getPassword.mkString,agentNameField.get.text,keyFileDir)
        keyWin = None
        window.get.peer.revalidate()
        activePane.get.requestFocusInWindow()
      case "5" =>
        val pw1 = keyWin.get.passwordField.get.peer.getPassword.mkString
        val pw2 = keyWin.get.confirmPasswordField.get.peer.getPassword.mkString
        if (keyWin.get.newKey) {
          if (pw1 == pw2 && pw1.length>=6) {
            keyWin.get.readyButton.get.enabled = true
          } else {
            keyWin.get.readyButton.get.enabled = false
          }
        } else {
          if (pw1.length>=6) {
            keyWin.get.readyButton.get.enabled = true
          } else {
            keyWin.get.readyButton.get.enabled = false
          }
        }
      case _ =>
    }
  }
}