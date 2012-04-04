package ppl.delite.runtime
package profiler

import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import scala.io.Source
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.DeliteTaskGraph
import ppl.delite.runtime.graph.ops._
import scala.util.parsing.json.JSON

/**
 * Manages, inputs, and outputs profiling data.
 * 
 * @author Philipp Haller
 */
object Profiler {
  
  var sourceInfo: Map[String, (String, Int, String)] = Map()
  var graph: Option[DeliteTaskGraph] = None
    
  def getFieldMap(map: Map[Any, Any], field: String): Map[Any,Any] = {
    map.get(field) match {
      case Some(field) => field match {
        case map: Map[Any,Any] => map
        case err => error("JSON map not found")
      }
      case None => error("JSON field not found")
    }
  }

  def getFieldMapOption(map: Map[Any, Any], field: String): Option[Map[Any, Any]] = {
    map.get(field) match {
      case Some(field) => field match {
        case map: Map[Any, Any] => Some(map)
        case err => None
      }
      case None => None
    }
  }
  
  def getFieldList(map: Map[Any, Any], field: String): List[Any] = {
    map.get(field) match {
      case Some(field) => field match {
        case list: List[Any] => list
        case err => error("JSON list not found")
      }
      case None => error("JSON field not found")
    }
  }

  def getFieldString(map: Map[Any, Any], field:String): String = {
    map.get(field) match {
      case Some(field) => field.toString
      case None => error("field not found")
    }
  }

  def getFieldListofLists(map: Map[Any, Any], field: String): List[List[Any]] = {
    map.get(field) match {
      case Some(field) => field match {
        case llist: List[List[Any]] => llist
        case err => error("JSON list of lists not found")
      }
      case None => error("JSON field not found")
    }
  }

  def mapFromParsedJSON(json: Any): Map[String, List[List[(String, String, Int)]]] = {
    val mappings = json match {
      case m: Map[String, List[List[Map[Any,Any]]]] => m
      case err => error("JSON map not found")
    }

    Map[String, List[List[(String, String, Int)]]]() ++ (for ((symbolName, sContextss) <- mappings) yield {

      // parse source contexts
      val value : List[List[(String, String, Int)]] = 
        for(sContexts <- sContextss) yield {
          for(sContext : Map[Any, Any] <- sContexts) yield {
            (getFieldString(sContext, "fileName"), getFieldString(sContext, "opName"), getFieldString(sContext, "line").toInt)
          }
        }
      (symbolName -> value)
    })
  }

  def createTaskList(json: Any) : List[(String, Int)] = {
    val taskList = json match {
      case m: List[Map[Any, Any]] => m
      case err => error("JSON map not found")
    }

    (for (elem <- taskList) yield {
      (getFieldString(elem, "kernel"), getFieldString(elem, "duration").toInt)
    }).toList
   
  }  
  
  def init(info: Map[String, (String, Int, String)], taskGraph: DeliteTaskGraph) {
    sourceInfo = info
    graph = Some(taskGraph)
  }
  
  def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }
  
  def deliteOpByIdOrNone(id: String): Option[DeliteOP] =
    graph.get.ops.find(_.id == id)
  
  def deliteOpType(id: String): String = {
    val opOpt = deliteOpByIdOrNone(id)
    if (opOpt.isEmpty) "ALL"
    else {
      opOpt.get match {
        case _: OP_Single => "OP_Single"
        case _: OP_External => "OP_External"
        case _: OP_MultiLoop => "OP_MultiLoop"
        case _: OP_Foreach => "OP_Foreach"
        case other =>
          if (other.toString() == "x0") "ARGS"
          else if (other.toString() == "eop") "EOP"
          else //error("OP Type not recognized: " + other)
            ""
      }
    }
  }
  
  def safeSourceInfo(timing: Timing) =
    sourceInfo.get(timing.component) match {
      case None => ("&lt;unknown file&gt;", 0, timing.component)
      case Some(tuple) => tuple
    }
  
  def iterableToJSArray[T](arrName: String, values: Iterable[T], quoted: Boolean = true): String = {
    val arr = (for (v <- values) yield (if (quoted) "\"" + v + "\"" else v)
               ).mkString("[", ", ", "]")
    "var " + arrName + " = " + arr + ";"
  }
  
  def listOfListsToJSArray[T](arrName: String, values: List[List[T]]): String = {
    val arr = (for (v <- values) yield v.mkString("[", ", ", "]")).mkString("[", ", ", "]")
    "var " + arrName + " = " + arr + ";"
  }
  
  trait TaskInfo {
    val fromTiming: Timing
    val startNanos: Long
    val duration: Long
    val kernel: String
    val location: Int
    val line: String
    val cssline: String
    val tooltip: String
  }
  
  object TaskInfo {
    def apply(timing: Timing, threadId: Int, globalStartNanos: Long): TaskInfo =
      new TaskInfo {
        val fromTiming = timing
        val duration =   timing.elapsedMicros
        val startNanos = (timing.startTime - globalStartNanos) / 1000
        val kernel =     safeSourceInfo(timing)._3
        val location =   threadId
        val line = {
          val (fileName, line, opName) = Profiler.sourceInfo.get(timing.component) match {
            case None =>        ("&lt;unknown file&gt;", 0, timing.component)
            case Some(tuple) => tuple
          }
          relativePath(fileName)+":"+line
        }       

        val cssline = {
          val (fileName, line, opName) = Profiler.sourceInfo.get(timing.component) match {
            case None =>        ("&lt;unknown file&gt;", 0, timing.component)
            case Some(tuple) => tuple
          }
          val baseName = relativePath(fileName)
          val noExtension = if(baseName.contains(".scala")) baseName.substring(0, baseName.length - 6) else baseName 
          noExtension + "_" + line
        }

        val tooltip = {
          val html =
          "<b>Start time:</b> " + ((timing.startTime - globalStartNanos) / 1000) + "us</br>" +
          "<b>Duration:</b> " + timing.elapsedMicros + "us</br>" +
          "<b>OPType:</b> " + deliteOpType(timing.component) + "</br>" +
          "<b>Kernel:</b> " + timing.component + "</br>" +
          "<b>Source:</b> " + line
          "'" + html + "'"
        }
      }
  }


  /**
   * emits data arrays to a writer, also emits task information in JSON format
   * for later use
   */
  def emitProfileDataArrays(globalStartNanos: Long, stats: Map[String, List[Timing]], writer: PrintWriter) {
/*
var duration = [280, 800, 400, 500, 1500, 600];
var start = [1000, 10, 800, 820, 2, 200];
var kernels = ["x174", "x277", "x107", "x135", "x108", "x99"];
var location = [0, 1, 2, 1, 3, 0];
var line_in_source = ['&lt;unknown file&gt;:0','HelloWorld4.scala:8','HelloWorld4.scala:9','HelloWorld4.scala:12','HelloWorld4.scala:14','&lt;unknown file&gt;:0'];
var tooltip = ['<b>Start time: </b>1000us </br><b>Duration:</b> 280us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>10us </br><b>Duration:</b> 800us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>800us </br><b>Duration:</b> 400us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>820us </br><b>Duration:</b> 500us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>2us </br><b>Duration:</b> 1500us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>200us </br><b>Duration:</b> 600us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9'];

var res = ["T0", "T1", "T2", "T3", "T4", "T5"];

var parallelTasks = [[1, 4, 6], [2, 3, 5]];
*/
    val allInitTimings = (for (id <- stats.keys; timing <- stats(id)) yield timing).toList

    // for skipping input
    val globalAppStartNanos = stats.get("app") match {
      case Some(appStats) =>
      	//println("found stats for app")
        val appStart = appStats(0).startTime
        //println("app start nanos: "+appStart)
      	appStart
      case None =>
        globalStartNanos        
    }
    
    // only include timings started after globalAppStartNanos
    val allTimings = allInitTimings filter { _.startTime >= globalAppStartNanos }
    
    val threads = (allTimings.flatMap { timing =>
      timing.threadName :: (timing match {
        case mt: MultiTiming => mt.timings.map(_.threadName).toList
        case other => List()
      })
    }).distinct
    val threadId: Map[String, Int] = Map() ++ (for (elem <- threads zipWithIndex) yield elem)
    
    // output res array (TODO: improve names)
    val resJS = iterableToJSArray("res", (for ((t, i) <- threads.zipWithIndex) yield "T"+i))
    writer.println(resJS)
    
    val initTaskInfos =  allTimings map { timing => TaskInfo(timing, threadId(timing.threadName), globalAppStartNanos) }
    
    // expand task infos to include chunk tasks
    val taskInfos = initTaskInfos flatMap { taskInfo =>
      taskInfo :: (taskInfo.fromTiming match {
        case mt: MultiTiming =>
          (mt.timings map { chunkTiming =>
            TaskInfo(chunkTiming, threadId(chunkTiming.threadName), globalAppStartNanos)
          }).toList
        case other => List()
      })
    }

    val parallelTaskIndices = taskInfos map { taskInfo =>
      val component = taskInfo.fromTiming.component
      // generate list of parallel task infos (their indices)
      ((taskInfos zipWithIndex) filter {
        case (info, _) => info.fromTiming.component == component
      }).map(_._2)
    }
    
    val durationJS =   iterableToJSArray("duration", taskInfos.map(_.duration), false)
    val startNanosJS = iterableToJSArray("start", taskInfos.map(_.startNanos), false)
    val kernelsJS =    iterableToJSArray("kernels", taskInfos.map(_.kernel))
    val locationsJS =  iterableToJSArray("location", taskInfos.map(_.location), false)
    val linesJS =      iterableToJSArray("line_in_source", taskInfos.map(_.line).map("'" + _ + "'"), false)
    val csslinesJS =      iterableToJSArray("cssline_in_source", taskInfos.map(_.cssline).map("'" + _ + "'"), false)
val tooltipsJS =   iterableToJSArray("tooltip", taskInfos.map(_.tooltip), false)
    val parallelTasksJS = listOfListsToJSArray("parallelTasks", parallelTaskIndices)
    
    writer.println(durationJS)
    writer.println(startNanosJS)
    writer.println(kernelsJS)
    writer.println(locationsJS)
    writer.println(linesJS)
    writer.println(csslinesJS)
    writer.println(tooltipsJS)
    writer.println(parallelTasksJS)
  }

  /**
   * emits kernel information for later use in JSON format
   * also emits a map of symbols to kernel files to a different file
   */
  
  def emitTaskInfo(globalStartNanos: Long, stats: Map[String, List[Timing]], stream: PrintWriter, symWriter: PrintWriter) {
     // for skipping input
    val globalAppStartNanos = stats.get("app") match {
      case Some(appStats) =>
      	//println("found stats for app")
        val appStart = appStats(0).startTime
        //println("app start nanos: "+appStart)
      	appStart
      case None =>
        globalStartNanos
    }

    val kernelsToDuration: Map[String, Long] = (for((id,timings) <- stats; if(new File(Config.buildDir+"/scala/kernels", id+".scala").exists)) yield {
      // only include timings started after globalAppStartNanos
      val validTimings = timings filter { _.startTime >= globalAppStartNanos }
      val duration = validTimings.map(_.elapsedMicros).foldLeft(0:Long){_+_}
      (id,duration)
    }).filter{case (kernel, duration) => duration >0}

    
    //emit all data related to kernel files for sorting purposes
    var first = true
    stream.println("[")
    for((kernel,duration) <- kernelsToDuration; if duration > 0) {
      if(first) {first=  false} else stream.print(", ")
      stream.println("{")
      stream.print("\"kernel\": \"" + kernel + "\",")
      stream.print("\"duration\": \"" + duration + "\"")
      stream.println("}")
    }
    stream.println("]")

    
    val symToKernelMap = getSymToGenfilesMap(kernelsToDuration.keys.toList)

    //emit all data related to the symToKernel map
    first = true
    symWriter.println("{")
    for((symbol, (declSite, otherSites)) <- symToKernelMap) {
      if(first) {first=  false} else symWriter.print(", ")
      
      val declName = relativePath(declSite)
      val noExtension = if(declName.contains(".scala")) declName.substring(0, declName.length - 6) else declName 

      symWriter.print("\"" + symbol + "\" : " + "{")
      symWriter.print("\"declSite\" : \""+ noExtension + "\",")
      
      symWriter.print("\"otherSites\" : [")
      var first2 = true
      for(otherSite <- otherSites){
        if(first2){first2 = false}else{symWriter.print(",")}
        val otherName = relativePath(otherSite)
        val noExtension = if(otherName.contains(".scala")) otherName.substring(0, otherName.length - 6) else otherName 
        symWriter.print("\""+noExtension+"\"")
      }
      symWriter.println("]")
      symWriter.println("}")
    }
    symWriter.println("}")
  }


  /**
   * create a map from a symbol to a generated file
   * symbol -> Map [declSite -> list, otherSites -> list]
   */
  def getSymToGenfilesMap(kernels: List[String]) : Map[String, (String, Set[String])] = {
    
    val result = new scala.collection.mutable.HashMap[String, (String, Set[String])]{

      def addMapping(sym: String, kernel: String, isDeclSite: Boolean) = {

        val symInfo = this.get(sym).getOrElse{ ("", scala.collection.immutable.Set[String]())}
        
        this(sym) = if(isDeclSite) (kernel, symInfo._2) else (symInfo._1, symInfo._2 + kernel)
        
      }
    }

    for(kernel <- kernels){
      val kernelFile = new File(Config.buildDir +"/scala/kernels", kernel+".scala")
      val lines = Source.fromFile(kernelFile).getLines

      val symbolRegex = "val (x\\d+)|x\\d+".r
      for(line <- lines){
        val syms = symbolRegex.findAllIn(line)
        for(sym <- syms){
          val isDeclSite = !sym.startsWith("x")
          val realSym = if(!isDeclSite) sym else sym.split(" ")(1)
          result.addMapping(realSym, kernelFile.getCanonicalPath(), isDeclSite) 
        }
      }
    }

    result.toMap
  }

  /**
   * emits profile data contained in the stats map
   * visualizable info is printed to fileName
   * kernel information and duration is printed to taskFileName
   */
  def emitProfileData(dir: File, fileName: String, taskFileName: String, symFileName: String, globalStartNanos: Long, stats: Map[String, List[Timing]]) {
    val dataFile = new File(dir, fileName)
    val fileWriter = new FileWriter(dataFile)
    val writer = new PrintWriter(fileWriter)
    
    def emitProperties(props: List[String]) {
      props.foreach(prop => writer.println("profileDataObj." + prop + " = " + prop + ";"))
	  }

   
    writer.println("function profileData() {")
    emitProfileDataArrays(globalStartNanos, stats, writer)
    writer.println("var profileDataObj = new Object();")
    emitProperties(List("res", "duration", "start", "kernels", "location", "line_in_source", "cssline_in_source", "tooltip"))
    writer.println("return profileDataObj; }")
    
    writer.flush()
    fileWriter.flush()
    fileWriter.close()

    //emitting kernelInfo to taskWriter
    val taskDataFile = new File(dir, taskFileName)
    val taskFileWriter = new FileWriter(taskDataFile)
    val taskWriter = new PrintWriter(taskFileWriter)

    val symDataFile = new File(dir, symFileName)
    val symFileWriter = new FileWriter(symDataFile)
    val symWriter = new PrintWriter(symFileWriter)
 
    emitTaskInfo(globalStartNanos, stats, taskWriter, symWriter)

    taskWriter.flush()
    taskFileWriter.flush()
    taskFileWriter.close()

    symWriter.flush()
    symFileWriter.flush()
    symFileWriter.close()
  }

 /**
   * emits profile data contained in the stats map
   * visualizable info is printed to fileName
   * kernel information and duration is printed to taskFileName
   */
  def emitLineidMap(dir: File, fileName: String, lineIds: Map[Lineid, Set[String]]) {
    val dataFile = new File(dir, fileName)
    val fileWriter = new FileWriter(dataFile)
    val writer = new PrintWriter(fileWriter)
    

    writer.println("{")
    var first = true;
    for((line, kernels) <- lineIds){
      if(first){first = false}else{writer.print(", ")}
      writer.print("\""+line+"\"" + " : ")
      writer.print("[");
      var first2 = true;
      for(kernel <- kernels){
        if(first2){first2 = false}else{writer.print(", ")}
        writer.print("\"" + kernel + "\"")
      }
      writer.println("]")
    
    }
    writer.println("}")

    writer.flush()
    fileWriter.flush()
    fileWriter.close()

  }
 
  /** Writes profile to JavaScript file (profileData.js).
   *  also emits a JSON map (taskInfos.json) which will be used to print
   *  the right sources when running the visualizer
   *  
   *  Requires system property stats.output.dir to be set.
   */
  def writeProfile(globalStart: Long, globalStartNanos: Long, stats: Map[String, List[Timing]]) {
    val directory = getOrCreateOutputDirectory()
    // emit JS file containing the profile data
	  emitProfileData(directory, "profileData.js", "taskInfos.json", "symstokernels.json", globalStartNanos, stats)

  }
  
  def writeProfile(globalStart: Long, stats: Map[String, List[Timing]], writer: PrintWriter) {
    for (id <- stats.keys) {
      val timings = stats(id).flatMap(p => {
        val postfix = if (p.isInstanceOf[MultiTiming]) {
          "MultiLoop: " + p.asInstanceOf[MultiTiming].timings.mkString(" ")
        } else
          ""
        
        val source = sourceInfo.get(id) match {
          case None => "&lt;unknown file&gt;"
          case Some((fileName, line, opName)) => fileName + ":" + line
        }
        
        val microsElapsed = (p.endTime - p.startTime) / 1000        
        List(p.threadName, microsElapsed + "us", postfix, source)
      })
      writer.println(id + " " + timings.mkString(" "))
    }
    writer.flush()
  }
  
  /** Generates HTML profile.
   *  
   *  Requires system properties stats.output.dir, stats.output.filename,
   *  and delite.deg.filename to be set.
   *  
   *  Requires that the following files have been generated: symbols.json, profileData.js
   */
  def main(args: Array[String]) {

    // only generate HTML profile
    // TODO: check that symbols.json and profileData.js have already been generated
	
    // read symbol source info from file
    val symbolsFilename = new File(getOrCreateOutputDirectory(), "symbols.json")

    val contents = scala.io.Source.fromFile(symbolsFilename).mkString
    
    // maps a symbol (name) to its source context(s): name -> List[List(fileName, opName, line)]]
    val symbolMap: Map[String, List[List[(String, String, Int)]]] =
      JSON.parseFull(contents) match { // parse JSON into map
        case Some(json) => mapFromParsedJSON(json)
        case None => throw new RuntimeException("Couldn't parse the symbols file")
      }

    val lineidMap : Map[Lineid, Set[String]] = toLineidMap(symbolMap)
    //emit the lineidMap as json for later use
    emitLineidMap(getOrCreateOutputDirectory(), "lineids.json", lineidMap)

    // read taskInfoMap from file

    val taskInfoFilename = new File(getOrCreateOutputDirectory(), "taskInfos.json")
    val taskInfoContents = scala.io.Source.fromFile(taskInfoFilename).mkString

    // create a map for this
    val taskInfoList: List[(String, Int)] = 
      JSON.parseFull(taskInfoContents) match {
        case Some(json) => createTaskList(json)
        case _ => throw new RuntimeException("Couldn't parse the task info file")
      }
    
    val taskInfoSorted = taskInfoList.sortWith{ case (x,y) => x._2.compareTo(y._2) > 0}
    val htmlFile = new File(getOrCreateOutputDirectory(), Config.statsOutputFilename)
    val fileWriter = new PrintWriter(new FileWriter(htmlFile))
    Visualizer.writeHtmlProfile(fileWriter, symbolMap, taskInfoSorted.map(_._1))
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
  

  def toLineidMap(symbolMap : Map[String, List[List[(String, String, Int)]]]) : Map[Lineid, Set[String]] = {

    val result = new scala.collection.mutable.HashMap[Lineid, Set[String]] {

      def addMapping(context : (String, String, Int), sym : String) = {
        val lineid = Lineid(context._1, context._3)
        val listOfSymbols = this.get(lineid).getOrElse{ scala.collection.immutable.HashSet[String]() }
        this(lineid) = listOfSymbols + sym
      }
    }

    for((sym, contexts) <- symbolMap; lineage <- contexts; context <- lineage){
      result.addMapping(context, sym)
    }

    result.toMap
  }
}

/**
 * a Lineid represents a certain line number in a file
 */
case class Lineid(filename: String, line : Int){
  private val _noExtension = {
    val baseName = Profiler.relativePath(filename)
    if(baseName.contains(".scala")) baseName.substring(0, baseName.length - 6) else baseName
  }
  override def toString = _noExtension+"_"+line
}
