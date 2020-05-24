package prosomo.primitives

import prosomo.stakeholder.ActorRefWrapper

import scala.swing.{ListView, ScrollPane}

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
  }
  def refreshPeerList = {
    peerList.peer.setListData(peerSeq.toArray)
  }
}

