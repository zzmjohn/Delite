package ppl.delite.runtime.profiler

import collection.mutable
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import ppl.delite.runtime.Config

/** Measures execution times of various components.
  * 
  * @author Anand Atreya
  * @author Philipp Haller
  */
object PerformanceTimer
{
  val currentTimer = new mutable.HashMap[String, Long]
  // TODO: remove times and only use stats
  val times = new mutable.HashMap[String, mutable.ArrayBuffer[Double]]
  
  var stats: Map[String, List[(Long, Long)]] = Map()
  
  // TODO: use Platform instead of System
  // TODO: System.nanoTime() has lower overhead
  def start(component: String, printMessage: Boolean = true) {
    if (!times.contains(component)) {
      times += component -> new mutable.ArrayBuffer[Double]()
      stats += component -> List[(Long, Long)]()
    }
    if (printMessage) println("[METRICS]: Timing " + component + " #" + times(component).size + " started")
    val startTime = System.currentTimeMillis
    currentTimer += component -> startTime
    
    val previous = stats(component)
    val current = (startTime, 0l) :: previous
    stats += component -> current
  }

  // TODO: use Platform instead of System
  // TODO: System.nanoTime() has lower overhead
  def stop(component: String, printMessage: Boolean = true) {
    val endTime = System.currentTimeMillis
    stats(component) match {
      case (startTime, _) :: previousTimings =>
        val updatedTimings = (startTime, endTime) :: previousTimings
        stats += component -> updatedTimings
    }
    
    val x = (endTime - currentTimer(component)) / 1000D
    times(component) += x    
    if (printMessage) println("[METRICS]: Timing " + component + " #" + (times(component).size - 1) + " stopped")
  }

  def totalTime(component: String) {
    val total = times(component).toList.reduceLeft[Double](_+_)
    println("[METRICS]: total time for component " + component + ": " + total)
  }

  def clearAll() {
    for((k,v) <- times) {
      v.clear
    }
  }

  def print(component: String, globalStart: Long) {
    // special case for component == "prof"
    if (component == "prof") {
      writeProfile(globalStart)
    } else {
      val timeStr = times.get(component) map { "[METRICS]: Latest time for component " + component + ": " +  _.last.formatted("%.6f") + "s" }
      println(timeStr getOrElse "[METRICS]: No data for component " + component)
    }
  }

  def printProfile(globalStart: Long) {
    def inSecs(v: Long) = (v.toDouble / 1000d).formatted("%.6f")
    
    for (component <- stats.keys) {
      val timingsInSecs = stats(component) map { p =>
        (inSecs(p._1 - globalStart), inSecs(p._2 - globalStart))
      }
      
      println("[METRICS]: Timings for component " + component + ": " + timingsInSecs.mkString(" "))
    }
  }
  
  /** Writes profile to file provided using system properties.
    * Example: -Dstats.output.dir=profile -Dstats.output.filename=profile.txt
    */
  def writeProfile(globalStart: Long) {
    val directory = getOrCreateOutputDirectory()
    val timesFile = new File(directory, Config.statsOutputFilename)
    if (timesFile.exists)
      throw new RuntimeException("stats file " + timesFile + " already exists")
    val fileWriter = new PrintWriter(new FileWriter(timesFile))
    writeProfile(globalStart, fileWriter)
  }
  
  def writeProfile(globalStart: Long, writer: PrintWriter) {
    for (component <- stats.keys) {
      val timings = stats(component).flatMap(p => List(p._1 - globalStart, p._2 - globalStart))
      writer.println(component + " " + timings.mkString(" "))
    }
    writer.flush()
  }
  
  def getOrCreateOutputDirectory(): File = {
    // check that directory is there or make it
    val directory = new File(Config.statsOutputDirectory)
    if(directory.exists == false)
      directory.mkdirs
    else if(directory.isDirectory == false)
      throw new RuntimeException("statsOutputDirectory doesn't refer to a directory")
    directory
  }
  
  /**
   * dump stats to values provided by config parameters
   */
  def dumpStats() {
    assert(Config.dumpStats)
    dumpStats(Config.dumpStatsComponent)
  }

  def dumpStats(component: String) {
    val directory = getOrCreateOutputDirectory()
    val timesFile = new File(directory.getCanonicalPath + File.separator  + Config.statsOutputFilename)
    if(Config.dumpStatsOverwrite == false && timesFile.exists)
      throw new RuntimeException("stats file " + timesFile + " already exists")
    val fileStream = new PrintWriter(new FileWriter(timesFile))
    dumpStats(component, fileStream)
    fileStream.close
  }

  def dumpStats(component: String, stream: PrintWriter)  {
    times.get(component) map { _.map(_.formatted("%.2f")).mkString("\n") } foreach { stream.print }
  }

}