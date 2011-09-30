package ppl.delite.runtime
package profiler

import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph

object Profiler {
  
  var sourceInfo: Map[String, (String, Int, String)] = Map()
  var graph: Option[DeliteTaskGraph] = None
  
  def init(info: Map[String, (String, Int, String)], taskGraph: DeliteTaskGraph) {
    sourceInfo = info
    graph = Some(taskGraph)
  }
  
  /** Writes profile to file provided using system properties.
   * 
   *  Example: -Dstats.output.dir=profile -Dstats.output.filename=profile.txt
   */
  def writeProfile(globalStart: Long, stats: Map[String, List[Timing]]) {
    val directory = getOrCreateOutputDirectory()
    val timesFile = new File(directory, Config.statsOutputFilename)
    // currently, the timesFile is overwritten for every run
    // TODO: detect existing files, and create new ones with -2, -3, etc. appended
    // TODO: the visualization should be able to use such a series of files
    val fileWriter = new PrintWriter(new FileWriter(timesFile))
    //writeProfile(globalStart, stats, fileWriter)
    Visualizer.writeHtmlProfile(globalStart, stats, fileWriter)
  }
  
  def writeProfile(globalStart: Long, stats: Map[String, List[Timing]], writer: PrintWriter) {
    for (id <- stats.keys) {
      val timings = stats(id).flatMap(p => {
        val postfix = if (p.isInstanceOf[MultiTiming]) {
          "MultiLoop: " + p.asInstanceOf[MultiTiming].timings.mkString(" ")
        } else
          ""
        
        val source = sourceInfo.get(id) match {
          case None => "<unknown file>"
          case Some((fileName, line, opName)) => fileName + ":" + line
        }
        
        val microsElapsed = (p.endTime - p.startTime) / 1000        
        List(p.threadName, microsElapsed + "us", postfix, source)
      })
      writer.println(id + " " + timings.mkString(" "))
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
  
}
