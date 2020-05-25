package prosomo.primitives

import prosomo.stakeholder.ActorRefWrapper

import scala.swing.{ColorChooser, ListView, ScrollPane, TextArea}
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.awt.Color

import com.google.common.base.{Splitter, Strings}
import com.google.common.collect.Iterables

import scala.swing.Font.Style

object SharedData extends Types {
  var counter = 0
  var errorFlag = false
  var killFlag = false
  var txCounter = 0
  var printingHolder = 0
  var t_0:Long = 0
  var t_1:Long = 0
  var timing:Boolean = false
  var diskAccess:Boolean = false


  val outText = new ByteArrayOutputStream
  val printStream = new PrintStream(outText)
  val oldOut = System.out

  def time0 = {
    timing = true
    t_0 = System.nanoTime()
  }

  def time1 = if (timing) {
    t_1 = System.nanoTime()
    timing = false
    val outTime = (t_1 - t_0)*1.0e-9
    val tString = "%6.6f".format(outTime)
    println("Elapsed time: " + tString + " s")
  }

  def count:Int = {
    val out = counter
    counter += 1
    println(counter)
    out
  }

  def throwDiskWarning(text:String) = if (!diskAccess) {
    println(Console.YELLOW + s"Disk Access: $text" + Console.RESET)
    diskAccess = true
  }

  def throwError(id:Int) = {println(s"Holder $id <---------Error------------<<<<")}
  def throwError = {println("<---------Error------------<<<<")}
  def error:Boolean = {errorFlag}

  var guiPeerInfo:Map[String,List[ActorRefWrapper]] = Map()
  def peerSeq:Seq[String] = {
    var out:Seq[String] = Seq()
    for (entry<-guiPeerInfo) {
      out ++= Seq(entry._1)
      out ++= entry._2.map("  "+_.actorPath.toString+"  ")
    }
    out
  }
  var peerList = new ListView(peerSeq) {
    renderer = ListView.Renderer(entry=>entry)
    background = Color.getHSBColor(1.0.toFloat,0.0.toFloat,0.15.toFloat)
    foreground = Color.getHSBColor(0.43.toFloat,0.6.toFloat,0.7.toFloat)
  }
  def refreshPeerList = {
    peerList.peer.setListData(peerSeq.toArray)
  }
  var outputText = new ColorTextArea {
    editable = false
    font = swing.Font("Monospaced",Style.Plain,12)
    background = Color.getHSBColor(1.0.toFloat,0.0.toFloat,0.15.toFloat)
    foreground = Color.getHSBColor(0.43.toFloat,0.6.toFloat,0.7.toFloat)
    lineWrap = true
  }

  var outputElem = new ScrollPane(outputText) {
    verticalScrollBar.value = verticalScrollBar.maximum
  }

  def refreshOutput = {
    outputText.appendANSI(outText.toString)
    outText.reset()
    if (!outputElem.verticalScrollBar.valueIsAdjusting) outputElem.verticalScrollBar.value = outputElem.verticalScrollBar.maximum
  }

}

