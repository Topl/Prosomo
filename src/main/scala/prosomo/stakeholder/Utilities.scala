package prosomo.stakeholder

import prosomo.primitives.Types

trait Utilities extends Members with Types {

  import prosomo.primitives.Parameters._

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

  def containsDuplicates(s:Map[String,String]):Boolean = {
    var s1:List[String] = List()
    var s2:List[String] = List()
    for (entry <- s) {
      s1 ++= List(entry._1)
      s2 ++= List(entry._2)
    }
    (s1.distinct.size != s1.size) && (s2.distinct.size != s2.size)
  }

  /**
    * utility for timing execution of methods
    * @param block any execution block
    * @tparam R
    * @return
    */
  def timeFlag[R](block: => R): R = {
    if (timingFlag && holderIndex == 0) {
      val t0 = System.nanoTime()
      val result = block
      val t1 = System.nanoTime()
      val outTime = (t1 - t0)*1.0e-9
      if (outTime > slotT*1000000L*1.0e-9) {
        val tString = "%6.6f".format(outTime)
        println(Console.YELLOW  + "Warning: method call elapsed time: " + tString + "s longer than slot time" + Console.WHITE)
      }
      result
    } else {
      block
    }
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val outTime = (t1 - t0)*1.0e-9
    val tString = "%6.6f".format(outTime)
    println("Elapsed time: " + tString + " s")
    result
  }

}
