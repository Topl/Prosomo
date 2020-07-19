package prosomo

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, Dimension}
import java.io.File
import akka.actor.ActorRef
import com.typesafe.config.{Config, ConfigFactory}
import javax.swing.{BorderFactory, JOptionPane, SwingUtilities}
import prosomo.cases.NewHolderFromUI
import prosomo.components.Serializer
import prosomo.primitives.Parameters.{devMode, fch}
import prosomo.primitives._
import scorex.util.encode.Base58
import scala.swing.Font.Style
import scala.swing._
import scala.swing.event.{ButtonClicked, InputEvent, KeyReleased}
import scala.util.Try

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

  import javax.swing.UIManager
  val peerDiscoveryAddress = "34.72.8.173:9084"
  val logo = Try{new javax.swing.ImageIcon(getClass.getClassLoader.getResource("Logo.png"))}.toOption
  val icon = Try{new javax.swing.ImageIcon(getClass.getClassLoader.getResource("Icon.png"))}.toOption

  Try{
    System.setProperty("apple.laf.useScreenMenuBar", "true")
    System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Prosomo")
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  }

  var windowConfig:Config = config
  var waitToConnect = true
  var runApp = true
  val listener = this

  var coordRef:ActorRef = _

  def uuid: String = java.util.UUID.randomUUID.toString

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
    new Button("Issue Command") {}
  }.toOption

  val commandElem = Try{
    new BoxPanel(Orientation.Horizontal) {
      contents += cmdField.get
      contents += cmdButton.get
    }
  }.toOption

  val backgroundC = Try{Color.getHSBColor(1.0.toFloat,0.0.toFloat,0.15.toFloat)}
  val foregroundC = Try{Color.getHSBColor(0.46.toFloat,0.6.toFloat,0.7.toFloat)}

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
      background = backgroundC.get
      foreground = foregroundC.get
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
    }
  }.toOption

  Try{
    upnpCheck.get.reactions += {
      case event.ButtonClicked(_) => if (upnpCheck.get.selected) {
        declaredAddressField.get.enabled = false
        declaredAddressField.get.text = ""
        declaredAddressField.get.peer.setOpaque(false)
      } else {
        declaredAddressField.get.enabled = true
        declaredAddressField.get.text = uiDeclaredAddress
        declaredAddressField.get.peer.setOpaque(true)
      }
    }
  }

  val connectButton = Try{
    new Button("Connect") {
      enabled = false
      tooltip = "Enter a name below before you connect"
      reactions += {
        case event.ButtonClicked(_) => {
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
        }
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
          peer.setOpaque(false)
        }
        contents += knownAddressField.get
        contents += new TextField("  Declared address: ") {
          editable=false
          maximumSize = new Dimension(125,50)
          minimumSize = new Dimension(125,50)
          border=BorderFactory.createEmptyBorder()
          peer.setOpaque(false)
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

  /********************************************************************************************************************/

  var txWin:Option[IssueTxWindow] = None

  val issueTxButton = Try{
    new Button ("Issue Transaction") {
      enabled = false
      reactions += {
        case scala.swing.event.ButtonClicked(_) => {
          enabled = false
          txWin match {
            case None =>
            case Some(_) => txWin = None
          }
          txWin = Try{
            new IssueTxWindow(SharedData.issueTxInfo match {
              case Some(info) => Base58.encode(info._1.data)
              case None => ""
            },SharedData.issueTxInfo.get._2.toSeq.map {
              info =>
                val pks = info._2._2
                val pkw: String = Base58.encode(pks._1 ++ pks._2 ++ pks._3)
                info._2._1.actorPath.toString() + " " + pkw
            })
          }.toOption
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
      reactions += {
        case scala.swing.event.ButtonClicked(_) =>
          confirmSendToNetworkWindow.get.open()
      }
    }
  }.toOption

  val sendTxButton = Try {
    new Button ("Send")
  }.toOption

  val confirmSendToNetworkWindow = Try {
    new Frame {
      title = "Confirm"
      iconImage = icon.get.getImage
      contents = new BorderPanel {
        border = Swing.EmptyBorder(10, 10, 10, 10)
        add(sendTxButton.get,BorderPanel.Position.East)
        add(new Button ("Cancel") {
          reactions += {
            case scala.swing.event.ButtonClicked(_) => {
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
        }
        contents += deltaField.get
        peer.setOpaque(false)
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
            }
            contents += recipField.get
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
              border=BorderFactory.createEmptyBorder()
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
              }
              contents += recipDropList.get
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
    }.toOption

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
      background = backgroundC.get
      foreground = foregroundC.get
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

  /********************************************************************************************************************/

  Try{
    javax.swing.UIManager.put("FileChooser.readOnly", true)
  }

  val keyLocation = Try{config.getString("params.keyFileDir")}.toOption

  val keyDir = Try{new File(keyLocation.get)}.toOption

  Try{keyDir.get.mkdirs()}

  val keysFileChooser = Try {
    new FileChooser(keyDir.get) {
      fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      listenTo(mouse.clicks)
      listenTo(mouse.moves)
      listenTo(mouse.wheel)
      listenTo(keys)
      reactions += {
        case event: InputEvent => listener.actionPerformed(new ActionEvent(this,2,"2"))
        case _ =>
      }
      peer.setControlButtonsAreShown(false)
    }
  }.toOption

  var keyFileDir = ""

  val newKeyButton = Try{
    new Button ("Create New Key") {
      reactions += {
        case ButtonClicked(_) => {
          val file = new File(keyLocation.get+"/"+agentNameField.get.text+"_"+System.currentTimeMillis().toString+"/")
          file.mkdirs()
          keysFileChooser.get.selectedFile = file
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
      reactions += {
        case ButtonClicked(_) =>
          keyWin match {
            case None =>
            case Some(win) => keyWin = None
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
      new TextField(if(newKey){"  Enter a new password, 6 characters or longer: "}else{"  Enter Password: "}) {
        editable = false
        border = BorderFactory.createEmptyBorder()
        peer.setOpaque(false)
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
      new TextField("  Confirm Password: ") {
        editable = false
        border = BorderFactory.createEmptyBorder()
        peer.setOpaque(false)
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
      }
    }.toOption

    val bip39Field = Try{
      new TextField("") {
        columns = 120
      }
    }

    val bip39 = Bip39("en")

    val readyButton = Try{
      new Button (if(newKey){"Generate"}else{"Load"}) {
        if (newKey) enabled = false
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
          }
          if (newKey) contents += new BoxPanel(Orientation.Horizontal) {
            contents += confirmHelp.get
            contents += confirmPasswordField.get
          }
          if (newKey) contents += {
            new BoxPanel(Orientation.Horizontal) {
              contents += bip39Help.get
              contents += new Button ("Seed New Phrase") {
                reactions += {
                  case scala.swing.event.ButtonClicked(_) =>
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
            }
          }
          if (newKey) contents += bip39Field.get
          contents += new BoxPanel(Orientation.Horizontal) {
            contents += readyButton.get
            contents += new Button ("Cancel") {
              reactions += {
                case scala.swing.event.ButtonClicked(_) => {
                  close()
                }
              }
            }
          }
        }
        if (newKey) {
          minimumSize = new Dimension(800,200)
        }
        readyButton.get.reactions += {
          case scala.swing.event.ButtonClicked(_) =>
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
          border=BorderFactory.createEmptyBorder()
          peer.setOpaque(false)
        }
        contents += loadKeyButton.get
        contents += newKeyButton.get
      }
      contents += keysFileChooser.get
      border=BorderFactory.createEmptyBorder()
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
        case _: InputEvent => listener.actionPerformed(new ActionEvent(this,1,"1"))
        case _ =>
      }
    }
  }

  val newNodeViewButton = Try{
    new Button ("Create Database") {
      reactions += {
        case ButtonClicked(_) => {
          val file = new File(dataDir+"/"+agentNameField.get.text+"_"+System.currentTimeMillis().toString+"/")
          file.mkdirs()
          nodeViewFileChooser.get.selectedFile = file
          nodeViewFileChooser.get.peer.rescanCurrentDirectory()
          dataLocation = nodeViewFileChooser.get.selectedFile.getPath
        }
      }
    }
  }.toOption

  val openNodeViewButton = Try {
    new Button ("Load Database") {
      enabled = false
      reactions += {
        case ButtonClicked(_) => {
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
          border=BorderFactory.createEmptyBorder()
          peer.setOpaque(false)
        }
        contents += openNodeViewButton.get
        contents += newNodeViewButton.get
      }
      contents += nodeViewFileChooser.get
      border=BorderFactory.createEmptyBorder()
    }
  }.toOption

  /********************************************************************************************************************/

  val nameHelp = Try{
    new TextField {
      text = "Enter your name or an alias with no special characters, then press Connect: "
      editable = false
      maximumSize = new Dimension(500,30)
      minimumSize = new Dimension(500,30)
      border=BorderFactory.createEmptyBorder()
      peer.setOpaque(false)
    }
  }.toOption

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

  val agentNameHelp = Try{
    new TextField {
      text = "Your agent name as seen by peers: "
      editable = false
      maximumSize = new Dimension(250,30)
      minimumSize = new Dimension(250,30)
      border=BorderFactory.createEmptyBorder()
      peer.setOpaque(false)
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
    }
  }.toOption

  val netStats = Try{
    new TextArea {
      text = ""
      editable = false
      border=BorderFactory.createEmptyBorder()
      background = backgroundC.get
      foreground = foregroundC.get
    }
  }.toOption

  val netStatsElem = Try{
    new ScrollPane(new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += nameHelp.get
        contents += nameField.get
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += agentNameHelp.get
        contents += agentNameField.get
      }
      contents += netStats.get
      border=BorderFactory.createEmptyBorder()
      maximumSize = new Dimension(2000,2000)
      preferredSize = new Dimension(800,400)
      minimumSize = new Dimension(100,100)
    })
  }.toOption

  /********************************************************************************************************************/

  val activePane = Try{
    new TabbedPane {
      pages += new TabbedPane.Page("Network Info",netStatsElem.get)
      pages += new TabbedPane.Page("Active Peers",peerListElem.get)
      pages += new TabbedPane.Page("Manage Database",nodeViewFileElem.get)
      pages += new TabbedPane.Page("Manage Keys",keysFileElem.get)
      tooltip = "Select a tab"
    }
  }.toOption

  Try {
    (1 to 3).foreach(activePane.get.pages(_).enabled = false)
  }

  Try {
    activePane.get.peer.setToolTipTextAt(0,"Network name and stats")
    activePane.get.peer.setToolTipTextAt(1,"List of discovered peers with stake info")
    activePane.get.peer.setToolTipTextAt(2,"Choose the blockchain data on disk")
    activePane.get.peer.setToolTipTextAt(3,"Load your keys from disk")
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
      if (devMode) contents += commandElem.get
      contents += connectElem.get
      contents += walletElem.get
      contents += activePane.get
      contents += outputElem.get
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
    }
  }.toOption

  def showUpnpWarn = Try{
    JOptionPane.showMessageDialog(windowContents.get.peer, "A Upnp device could not be found.\nTry connecting with Upnp disabled.\nIf you have access to your router,\nyou may have to manually forward ports.\nRestart the application to try again.", "Connection Failed", JOptionPane.WARNING_MESSAGE,logo.get)
  }

  window match {
    case None => Try{
      prosomo.primitives.Parameters.useGui = false
    }
    case _ => Try{
      while (waitToConnect) {
        Thread.sleep(100)
      }
      if (runApp) {
        nameField.get.enabled = false
        connectButton.get.enabled = false
        connectButton.get.text = "Connecting..."
        knownAddressField.get.editable = false
        declaredAddressField.get.editable = false
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
        activePane.get.pages(3).enabled = true
        activePane.get.pages(2).enabled = false
        activePane.get.peer.setSelectedIndex(3)
      case "4" =>
        keyWin.get.window.get.close()
        activePane.get.pages(3).enabled = false
        activePane.get.peer.setSelectedIndex(0)
        coordRef ! NewHolderFromUI(keyWin.get.windowKeyFile,dataLocation,keyWin.get.passwordField.get.peer.getPassword.mkString,agentNameField.get.text,keyFileDir)
        keyWin = None
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