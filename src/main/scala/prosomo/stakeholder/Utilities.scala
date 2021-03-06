package prosomo.stakeholder

import prosomo.primitives.Types

/**
  * AMS 2020:
  * Some useful and not so useful methods
  */

trait Utilities extends Members with Types {

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

  /**
    * utility for timing execution of methods
    * @param block any execution block
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
        println(Console.YELLOW  + "Warning: method call elapsed time " + tString + "s > slotT" + Console.RESET)
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
