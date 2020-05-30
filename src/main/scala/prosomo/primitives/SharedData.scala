package prosomo.primitives

import prosomo.stakeholder.ActorRefWrapper

import scala.swing.{ColorChooser, ListView, ScrollPane, TextArea}
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.awt.{Color, Dimension}

import com.google.common.base.{Splitter, Strings}
import com.google.common.collect.Iterables
import javax.swing.SwingUtilities
import prosomo.ProsomoWindow
import scorex.core.settings.ScorexSettings

import scala.swing.Font.Style
import scala.util.Try

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

  var prosomoWindow:Option[ProsomoWindow] = None

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

  var scorexSettings:Option[ScorexSettings] = None

  var guiPeerInfo:Map[String,List[ActorRefWrapper]] = Map()
  var confirmedBalance:Map[String,BigInt] = Map()
  var stakingBalance:Map[String,BigInt] = Map()
  var confirmedAlpha:Map[String,Double] = Map()
  var stakingAlpha:Map[String,Double] = Map()
  var walletInfo:(Int,Int,BigInt,BigInt) = (0,0,0,0)
}

