package prosomo.primitives

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

  def throwDiskWarning = if (!diskAccess) {
    println(Console.YELLOW + "Warning: Disk access" + Console.RESET)
    diskAccess = true
  }

  def throwError(id:Int) = {println(s"Holder $id <---------Error------------<<<<");errorFlag=true}
  def throwError = {println("<---------Error------------<<<<");errorFlag=true}
  def error:Boolean = {errorFlag}
}

