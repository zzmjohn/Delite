package ppl.delite.runtime.profiler

import java.io.{PrintWriter, FileInputStream}
import scala.io.Source
import ppl.delite.runtime.graph.ops._

case class TaskNode(timing: Timing, parent: Option[TaskNode])

/**
 * Generates visual, HTML-based profile
 * 
 * @author Philipp Haller
 */
object Visualizer {

  def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }
  
  def deliteOpById(id: String): DeliteOP =
    deliteOpByIdOrNone(id) match {
      case None => error("couldn't find DeliteOP with id " + id)
      case Some(op) => op
    }
  
  def deliteOpByIdOrNone(id: String): Option[DeliteOP] =
    Profiler.graph.get.ops.find(_.id == id)
  
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
          else error("OP Type not recognized: " + other)
      }
    }
  }
  
  def safeSourceInfo(timing: Timing) =
    Profiler.sourceInfo.get(timing.component) match {
      case None => ("<unknown file>", 0, timing.component)
      case Some(tuple) => tuple
    }
  
  // parent: parentTiming with same source context,
  // where op(timing) is an input of op(parentTiming)
  def isParentTimingOf(parentTiming: Timing, timing: Timing): Boolean = {
    safeSourceInfo(parentTiming) == safeSourceInfo(timing) && {
      val op = deliteOpById(timing.component)
      val parentOp = deliteOpById(parentTiming.component)
      !(parentOp.getInputs find {
        case (inputOp, _) => inputOp.id == op.id
      }).isEmpty
    }
  }
  
  def buildTaskTreeMap(timings: Map[String, List[Timing]]): Map[String, List[TaskNode]] = {
    
    // returns list with new node, other already-built nodes, as well as remaining timings
    def buildTaskNode(timing: Timing, nodes: List[TaskNode], remaining: List[Timing]): (TaskNode, List[TaskNode], List[Timing]) = {
      // search for parent in already-built nodes
      nodes.find(node => isParentTimingOf(node.timing, timing)) match {
        case Some(parentNode) =>
          val newNode = TaskNode(timing, Some(parentNode))
          (newNode, nodes, remaining)
        case None =>
          // search in remaining timings
          remaining.find(ptiming => isParentTimingOf(ptiming, timing)) match {
            case Some(parentTiming) =>
              val withoutParentTiming = remaining.filterNot(_ == parentTiming)
              val (parentNode, newNodes, newRemaining) = buildTaskNode(parentTiming, nodes, withoutParentTiming)
              val newNode = TaskNode(timing, Some(parentNode))
              (newNode, parentNode :: newNodes, newRemaining)
            case None =>
              // node without parent
              val newNode = TaskNode(timing, None)
              (newNode, nodes, remaining)
          }
      }
    }
    
    var remaining = timings.values.flatMap(l => l).toList
    var allNodes: List[TaskNode] = List()
    while (!remaining.isEmpty) {
      val (newNode, nodes, newRemaining) = buildTaskNode(remaining.head, allNodes, remaining.tail)
      allNodes = newNode :: nodes
      remaining = newRemaining
    }
    
    var taskTrees: Map[String, List[TaskNode]] = Map()
    for (thread <- timings.keys) {
      val tasks = allNodes.filter(_.timing.threadName == thread)
      taskTrees += (thread -> tasks)
    }
    taskTrees
  }
  
  def printSchedule() {
    // output schedule
    println("original schedule")
    for (res <- 0 until Profiler.graph.get.schedule.numResources) {
      // convert ArrayDequeue to List
      val dequeue = Profiler.graph.get.schedule(res)
      var l: List[DeliteOP] = List()
      val iter = dequeue.iterator()
      while (iter.hasNext()) l :::= List(iter.next())
      
      println(res + ": " + l.map(_.id).mkString(","))
    }
  }
  
  // rearranges timings according to the schedule
  def scheduledTimings(timings: List[Timing]): List[Timing] = {
/*
      val timing = timingsOf(t)(0)
      
      // find resource on which kernel is scheduled
      val res = (0 until Profiler.graph.get.schedule.numResources).find(resNum => {
        var found = false
        val iter = Profiler.graph.get.schedule(resNum).iterator()
        while (iter.hasNext() && !found) {
          val op = iter.next()
          if (op.id == timing.component)
            found = true
        }
        found
      }) match {
        case None => error("couldn't find resource")
        case Some(r) => r
      }
*/      
    // thread t corresponds to resource res
    // go through schedule of res, and add corresponding timing to list
    
    var sortedTimings: List[Timing] = List()
    for (res <- 0 until Profiler.graph.get.schedule.numResources) {
      val iter = Profiler.graph.get.schedule(res).iterator()
      while (iter.hasNext()) {
        val op = iter.next()
        timings.find(_.component == op.id) match {
          case None =>
            /* do nothing */
          case Some(timing) =>
            sortedTimings = sortedTimings ::: List(timing)
        }
      }
    }
    sortedTimings
  }
  
  // input: timings grouped by kernel id
  def scheduledTimingsMap(stats: Map[String, List[Timing]]): Map[String, List[Timing]] = {
    val threads = (for (id <- stats.keys; timing <- stats(id)) yield timing.threadName).toList.distinct
    
    // group timings per thread
    var timingsOf: Map[String, List[Timing]] = Map()
    for (t <- threads) {
      val timings =
        for (id <- stats.keys; timing <- stats(id); if timing.threadName == t) yield timing
      timingsOf += (t -> timings.toList)
    }
    
    printSchedule()
    
    // sort timings according to schedule
    var scheduled: Map[String, List[Timing]] = Map()
    for (t <- threads) {
      var sortedTimings = scheduledTimings(timingsOf(t))
      scheduled += (t -> sortedTimings)
    }
    scheduled
  }
  
  def after(t1: String, t2: String, ts: Iterable[String]): Boolean = {
    val iter = ts.iterator
    var seenFirst = false
    var seenSecond = false
    var res: Option[Boolean] = None
    while (iter.hasNext && res.isEmpty) {
      val t = iter.next()
      if (t == t1) seenFirst = true
      if (t == t2) seenSecond = true
      
      if (seenFirst && !seenSecond) res = Some(false)
      if (!seenFirst && seenSecond) res = Some(true)
    }
    if (res.isEmpty) res = Some(false)
    res.get
  }
  
  // adds holes based on the deps of a task
  // input: timings grouped by thread and sorted according to schedule 
  def generateSchedule(stats: Map[String, List[Timing]]): Map[String, List[Either[Boolean, Timing]]] = {
    println("enter generateSchedule")
    // timings in timingsOf are being scheduled and removed
    var timingsOf = scheduledTimingsMap(stats)
    println("timings sorted: "+timingsOf)
    
    var schedule: Map[String, List[Either[Boolean, Timing]]] = Map()
    var done: Map[String, Boolean] = Map() ++ timingsOf.keys.map(t => (t, false))
    var threadIter = timingsOf.keys.iterator
    var currThr = threadIter.next()
    var i = 0
    val maxIters = 10
    var timeStep = 0
    while (/*i < maxIters &&*/ done.values.exists(v => v == false)) {
      i += 1
      
      println("next iteration")
      // in this iteration:
      // append either Left(false) or Right(timing) to schedule of current thread
      
      // what tasks does the current thread still have?
      timingsOf(currThr) match {
        case List() =>
          // mark thread as done
          done += (currThr -> true)
          println("thread " + currThr + " is done")
            
        case timing :: rest =>
          // find out whether task can be scheduled
          // check dependencies
          val op = Profiler.graph.get.ops.find(_.id == timing.component) match {
            case None =>
              error("DeliteOp not found")
            case opt @ Some(_) =>
              println("found DeliteOP corresponding to timing " + timing)
              println("deps: " + opt.get.getDependencies.map(op => op.id).mkString(" "))
              opt.get
          }
          
          // check that all dependencies have been scheduled
          if (op.getDependencies forall { depOp =>
            depOp.id.endsWith("_h") ||
            (schedule exists {
              case (t, l) => l exists {
                case Right(tim) =>
                  depOp.id.startsWith(tim.component) && {
                    l.indexOf(Right(tim), 0) < timeStep || (after(t, currThr, timingsOf.keys))
                  }
                case Left(_) => false
              }
            })
          }) {
            // schedule timing
            println("scheduling " + timing + " on " + currThr)
            val currSchedule = schedule.getOrElse(currThr, List())
            schedule += (currThr -> (currSchedule ::: List(Right(timing))))
            val sizeBefore = timingsOf(currThr).length
            timingsOf += (currThr -> rest)
            val sizeAfter = timingsOf(currThr).length
            println("size before " + sizeBefore + " after " + sizeAfter)
          } else {
            // add hole
            println("adding hole on " + currThr)
            val currSchedule = schedule.getOrElse(currThr, List())
            schedule += (currThr -> (currSchedule ::: List(Left(false))))
          }
      }
      // go to next thread
      currThr = if (threadIter.hasNext) threadIter.next() else {
        // start from the beginning
        timeStep += 1
        threadIter = timingsOf.keys.iterator
        threadIter.next()
      }
    }
    
    schedule
  }
  
  def kernelsTable(show: Boolean = true, stats: Map[String, List[Timing]]) = {
    val table: scala.xml.Elem =
    <table>
    {
      // row per thread
      val threads = (for (id <- stats.keys; timing <- stats(id)) yield timing.threadName).toList.distinct
      /*
      var timingsOf: Map[String, List[Timing]] = Map()
      for (t <- threads) {
        val timings = for (id <- stats.keys; timing <- stats(id); if t == timing.threadName) yield timing
        timingsOf += (t -> timings.toList)
      }
      */
      val timingsOf = scheduledTimingsMap(stats)
      val schedule = generateSchedule(timingsOf).withDefault(thr => List())
      for (t <- threads) yield
        <tr>
          <td>{ t }</td>
          { println(schedule(t).mkString(" "))
            
            // generate TaskTree such that each thread is mapped to a List[TaskTree]: Map[String, List[TaskTree]]
            val taskTrees = buildTaskTreeMap(timingsOf)
            val timingsToDisplay =
              if (show) scheduledTimings(taskTrees(t).map(tree => tree.timing))
              else scheduledTimings(taskTrees(t).flatMap(tree => if (tree.parent.isEmpty) List(tree.timing) else List()))
            
            for (timing <- timingsToDisplay) yield {
              
              val (fileName, line, opName) = Profiler.sourceInfo.get(timing.component) match {
                case None => ("<unknown file>", 0, timing.component + " [" + deliteOpType(timing.component) + "]")
                case Some(tuple) => tuple
              }
              val source = relativePath(fileName) + ":" + line
              val showHideAction = "showHide('" + source + "');return false;"
              
              <td>
                <a href="#" onmouseover={ showHideAction } onmouseout={ showHideAction }>
                { opName + " (" + timing.elapsedMicros + "us)" }<span>{
                  "OpType: " + deliteOpType(timing.component) +
                  "\nSource: " + source + (if (timing.isInstanceOf[MultiTiming]) {
                    val multiTiming = timing.asInstanceOf[MultiTiming]
                    "\nMultiLoop: " + multiTiming.timings.mkString(" ") +
                    "\nImbalance: " + multiTiming.loadImbalance.formatted("%.3f")
                  } else "")
                }</span>
                </a>
              </td>
            }
          }
        </tr>
    }
    </table>
    
    if (show)
      <div id="kernels-show">{ table }</div>
    else
      <div id="kernels" class="hidden">{ table }</div>
  }
  
  def sourceViewHtml(stats: Map[String, List[Timing]]) = {
    <div><h2>Sources</h2>
    {
      val sourceFiles = (stats.keys.flatMap(id => {
        Profiler.sourceInfo.get(id) match {
          case None =>
            List()
          case Some((fileName, line, opName)) =>
            if (fileName.equals("<unknown file>")) List() else List(fileName)
        }
      })).toList.distinct
      // TODO: there must be a better way
      for (file <- sourceFiles;
           if !(file.contains("ppl/dsl") ||
                file.contains("ppl/delite") ||
                file.contains("lms-core"))) yield {
        <h3>{ file }</h3>
        <pre>
{
  // strip path from file name
  // id of single source line follows pattern "Source.scala:line"
  val baseName = relativePath(file)
  
  val source = Source.fromFile(file)
  val sourceLines = for ((line, num) <- source.getLines zipWithIndex) yield {
    val shownId =  baseName + ":" + (num + 1) + "-show"
    val hiddenId = baseName + ":" + (num + 1)
    <span id={ shownId }>{ (num + 1) + ":   " + line }</span>
    <span id={ hiddenId } class="hidden">{ (num + 1) + ":   " + line }</span>
    <br/>
  }
  sourceLines
}
        </pre>
      }
    }</div>
  }
  
  def generateHtmlProfile(globalStart: Long, globalStartNanos: Long, stats: Map[String, List[Timing]]) = {
    <html><head>
    <title>Delite Visual Profile</title>
    <link href="hover.css" rel="stylesheet" type="text/css"></link>
    <script src="showHide.js"></script>
    </head>
    <body>
    <h1>Delite Visual Profile</h1>
    <h2>Kernel Timings:</h2>{
      val showHideKernels = "showHide('kernels');return false;"
      <p>Show/hide synthetic kernels:
        <a href="#" onclick={ showHideKernels }>toggle</a>
      </p>
    }
    { kernelsTable(true, stats) }
    { kernelsTable(false, stats) }
    sourceViewHtml
    </body></html>
  }
  
  def replaceHtmlLineBreaks(s: String) =
    s.replaceAll("<br></br>", "<br/>")
  
  def copyFileTo(fileName: String, out: PrintWriter) {
	val source = Source.fromFile(fileName)
    for (line <- source.getLines) {
      out.println(line)
    }
  }

  def iterableToJSArray[T](arrName: String, values: Iterable[T], quoted: Boolean = true): String = {
    val arr = (for (v <- values) yield (if (quoted) "\"" + v + "\"" else v)
               ).mkString("[", ", ", "]")
    "var " + arrName + " = " + arr + ";"
  }
  
  trait TaskInfo {
    val fromTiming: Timing
    val startNanos: Long
    val duration: Long
    val kernel: String
    val location: Int
    val line: String
    val tooltip: String
  }
  
  object TaskInfo {
    def apply(timing: Timing, threadId: Int, globalStartNanos: Long): TaskInfo =
      new TaskInfo {
        val fromTiming = timing
        val duration =   timing.elapsedMicros
        val startNanos = (timing.startTime - globalStartNanos) / 1000
        val kernel =     timing.component
        val location =   threadId
        val line = {
          val (fileName, line, opName) = Profiler.sourceInfo.get(timing.component) match {
            case None =>        ("&lt;unknown file&gt;", 0, timing.component)
            case Some(tuple) => tuple
          }
          relativePath(fileName) + ":" + line
        }
        val tooltip = {
          val html =
          "<b>Start time:</b> " + ((timing.startTime - globalStartNanos) / 1000) + "us</br>" +
          "<b>Duration:</b> " + timing.elapsedMicros + "us</br>" +
          "<b>OPType:</b> " + deliteOpType(timing.component) + "</br>" +
          "<b>Source:</b> " + line
          "'" + html + "'"
        }
      }
  }
  
  def emitDataArrays(globalStartNanos: Long, stats: Map[String, List[Timing]], writer: PrintWriter) {
/*
var duration = [280, 800, 400, 500, 1500, 600];
var start = [1000, 10, 800, 820, 2, 200];
var kernels = ["x174", "x277", "x107", "x135", "x108", "x99"];
var location = [0, 1, 2, 1, 3, 0];
var line_in_source = ['&lt;unknown file&gt;:0','HelloWorld4.scala:8','HelloWorld4.scala:9','HelloWorld4.scala:12','HelloWorld4.scala:14','&lt;unknown file&gt;:0'];
var tooltip = ['<b>Start time: </b>1000us </br><b>Duration:</b> 280us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>10us </br><b>Duration:</b> 800us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>800us </br><b>Duration:</b> 400us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>820us </br><b>Duration:</b> 500us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>2us </br><b>Duration:</b> 1500us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9','<b>Start time: </b>200us </br><b>Duration:</b> 600us</br><b>OPType:</b> OP_Single </br><b>Source:</b> HelloWorld4.scala:9'];

var res = ["T0", "T1", "T2", "T3", "T4", "T5"];
*/
    
    val allTimings = (for (id <- stats.keys; timing <- stats(id)) yield timing).toList
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
    
    val initTaskInfos =  allTimings map { timing => TaskInfo(timing, threadId(timing.threadName), globalStartNanos) }
    
    // expand task infos to include chunk tasks
    val taskInfos = initTaskInfos flatMap { taskInfo =>
      taskInfo :: (taskInfo.fromTiming match {
        case mt: MultiTiming =>
          (mt.timings map { chunkTiming =>
            TaskInfo(chunkTiming, threadId(chunkTiming.threadName), globalStartNanos)
          }).toList
        case other => List()
      })
    }
    
    val durationJS =   iterableToJSArray("duration", taskInfos.map(_.duration), false)
    val startNanosJS = iterableToJSArray("start", taskInfos.map(_.startNanos), false)
    val kernelsJS =    iterableToJSArray("kernels", taskInfos.map(_.kernel))
    val locationsJS =  iterableToJSArray("location", taskInfos.map(_.location), false)
    val linesJS =      iterableToJSArray("line_in_source", taskInfos.map(_.line).map("'" + _ + "'"), false)
    val tooltipsJS =   iterableToJSArray("tooltip", taskInfos.map(_.tooltip), false)
    
    writer.println(durationJS)
    writer.println(startNanosJS)
    writer.println(kernelsJS)
    writer.println(locationsJS)
    writer.println(linesJS)
    writer.println(tooltipsJS)
  }

  def kernelsData(show: Boolean = true, stats: Map[String, List[Timing]]) = {
    val table: scala.xml.Elem =
    <table>
    {
      // row per thread
      val threads = (for (id <- stats.keys; timing <- stats(id)) yield timing.threadName).toList.distinct
      /*
      var timingsOf: Map[String, List[Timing]] = Map()
      for (t <- threads) {
        val timings = for (id <- stats.keys; timing <- stats(id); if t == timing.threadName) yield timing
        timingsOf += (t -> timings.toList)
      }
      */
      val timingsOf = scheduledTimingsMap(stats)
      val schedule = generateSchedule(timingsOf).withDefault(thr => List())
      for (t <- threads) yield
        <tr>
          <td>{ t }</td>
          { println(schedule(t).mkString(" "))
            
            // generate TaskTree such that each thread is mapped to a List[TaskTree]: Map[String, List[TaskTree]]
            val taskTrees = buildTaskTreeMap(timingsOf)
            val timingsToDisplay =
              if (show) scheduledTimings(taskTrees(t).map(tree => tree.timing))
              else scheduledTimings(taskTrees(t).flatMap(tree => if (tree.parent.isEmpty) List(tree.timing) else List()))
            
            for (timing <- timingsToDisplay) yield {
              
              val (fileName, line, opName) = Profiler.sourceInfo.get(timing.component) match {
                case None => ("<unknown file>", 0, timing.component + " [" + deliteOpType(timing.component) + "]")
                case Some(tuple) => tuple
              }
              val source = relativePath(fileName) + ":" + line
              val showHideAction = "showHide('" + source + "');return false;"
              
              <td>
                <a href="#" onmouseover={ showHideAction } onmouseout={ showHideAction }>
                { opName + " (" + timing.elapsedMicros + "us)" }<span>{
                  "OpType: " + deliteOpType(timing.component) +
                  "\nSource: " + source + (if (timing.isInstanceOf[MultiTiming]) {
                    val multiTiming = timing.asInstanceOf[MultiTiming]
                    "\nMultiLoop: " + multiTiming.timings.mkString(" ") +
                    "\nImbalance: " + multiTiming.loadImbalance.formatted("%.3f")
                  } else "")
                }</span>
                </a>
              </td>
            }
          }
        </tr>
    }
    </table>
    
    if (show)
      <div id="kernels-show">{ table }</div>
    else
      <div id="kernels" class="hidden">{ table }</div>
  }

  def writeHtmlProfile(globalStart: Long, globalStartNanos: Long, stats: Map[String, List[Timing]], writer: PrintWriter) {
    copyFileTo("profile-viz-top.html", writer)
    emitDataArrays(globalStartNanos, stats, writer)
    copyFileTo("profile-viz-mid.html", writer)
    val sourceHtmlNodes = sourceViewHtml(stats)
    val sourceHtmlAsString = sourceHtmlNodes.toString()
    // post process to replace <br></br>
    writer.println(replaceHtmlLineBreaks(sourceHtmlAsString))
    copyFileTo("profile-viz-bot.html", writer)
    writer.flush()
  }
}