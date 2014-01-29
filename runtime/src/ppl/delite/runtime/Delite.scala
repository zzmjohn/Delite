package ppl.delite.runtime

import codegen._
import executor._
import graph.ops.{EOP_Global, Arguments}
import graph.targets.Targets
import graph.{TestGraph, DeliteTaskGraph}
import profiler.{PerformanceTimer, Profiler}
import scheduler._
import tools.nsc.io._

/**
 * Author: Kevin J. Brown
 * Date: Oct 11, 2010
 * Time: 5:02:38 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Delite {

  private var mainThread: Thread = _
  private var outstandingException: Exception = _

  //TODO: Remove this. This is only used for cluster version GPU runtime code generation.
  var inputArgs: Array[String] = _

  private def printArgs(args: Array[String]) {
    if(args.length == 0) {
      println("Not enough arguments.\nUsage: [Launch Runtime Command] filename.deg arguments*")
      exit(-1)
    }
    println("Delite Runtime executing with the following arguments:")
    println(args.mkString(","))
  }

  private def printConfig() {
    println("Delite Runtime executing with: " + Config.numThreads + " Scala thread(s), " + Config.numCpp + " Cpp thread(s), " + Config.numCuda + " Cuda(s), " + Config.numOpenCL + " OpenCL(s)")
  }

  def main(args: Array[String]) {
    if (Config.clusterMode == 1) //master (scheduler)
      DeliteMesosScheduler.main(args)
    else if (Config.clusterMode == 2) //slave (executor)
      DeliteMesosExecutor.main(args)
    else
      embeddedMain(args, Map())
  }

  def embeddedMain(args: Array[String], staticData: Map[String,_]) {
    inputArgs = args
    mainThread = Thread.currentThread
    
    printArgs(args)
    printConfig()

    //extract application arguments
    Arguments.args = args.drop(1)
    Arguments.staticDataMap = staticData

    var scheduler: StaticScheduler = null
    var executor: Executor = null

    def abnormalShutdown() {
      if (executor != null) executor.shutdown()
      if (!Config.noRegenerate && !Config.alwaysKeepCache)
        Directory(Path(Config.codeCacheHome)).deleteRecursively() //clear the code cache (could be corrupted)
    }

    try {

      //load task graph
      val graph = loadDeliteDEG(args(0))
      //val graph = new TestGraph
    
      //Print warning if there is no op that supports the target
      if(Config.numCpp>0 && !graph.targets(Targets.Cpp)) { Config.numCpp = 0; println("[WARNING] No Cpp target op is generated!") }
      if(Config.numCuda>0 && !graph.targets(Targets.Cuda)) { Config.numCuda = 0; println("[WARNING] No Cuda target op is generated!") }
      if(Config.numOpenCL>0 && !graph.targets(Targets.OpenCL)) { Config.numOpenCL = 0; println("[WARNING] No OpenCL target op is generated!") }

      //TODO: combine into a single scheduler and executor
      scheduler = Config.scheduler match {
        case "SMP" => new SMPStaticScheduler
        case "ACC" => new Acc_StaticScheduler
        case "default" => {
          if (Config.numCpp+Config.numCuda+Config.numOpenCL==0 || ((graph.targets.size==1) && (graph.targets(Targets.Scala)))) new SMPStaticScheduler
          else if (Config.clusterMode == 1) new SMPStaticScheduler
          else new Acc_StaticScheduler
        }
        case _ => throw new IllegalArgumentException("Requested scheduler is not recognized")
      }

      executor = Config.executor match {
        case "SMP" => new SMPExecutor
        case "ACC" => new SMP_Acc_Executor
        case "default" => {
          if (Config.numCpp+Config.numCuda+Config.numOpenCL==0 || ((graph.targets.size==1) && (graph.targets(Targets.Scala)))) new SMPExecutor
          else if (Config.clusterMode == 1) new SMPExecutor
          else new SMP_Acc_Executor
        }
        case _ => throw new IllegalArgumentException("Requested executor is not recognized")
      }

      executor.init() //call this first because could take a while and can be done in parallel

      Config.deliteBuildHome = graph.kernelPath

      //load kernels & data structures
      loadSources(graph)

      //initialize profiler
      Profiler.init(sourceInfo, graph)
      
      //schedule
      scheduler.schedule(graph)

      //compile
      val executable = Compilers.compileSchedule(graph)

      //execute
      if (Config.clusterMode == 2) { //slave executor
        //DeliteMesosExecutor.executor = executor.asInstanceOf[SMPExecutor].threadPool
        if(executor.isInstanceOf[SMPExecutor])
          DeliteMesosExecutor.executor = executor.asInstanceOf[SMPExecutor].threadPool
        else {
          DeliteMesosExecutor.executor = executor.asInstanceOf[SMP_Acc_Executor].smpExecutor.threadPool
          executor.asInstanceOf[SMP_Acc_Executor].runAcc(executable)
        }
        DeliteMesosExecutor.awaitWork()
      }
      else { //master executor (including single-node execution)
        val numTimes = Config.numRuns
        for (i <- 1 to numTimes) {
          println("Beginning Execution Run " + i)
          val globalStart = System.currentTimeMillis
          val globalStartNanos = System.nanoTime()
          PerformanceTimer.start("all", false)
          // raghu
          println("[Delite] executable = " + executable)
          executor.run(executable)
          EOP_Global.await //await the end of the application program
          PerformanceTimer.stop("all", false)
          PerformanceTimer.printAll(globalStart, globalStartNanos)
          if (Config.dumpProfile) PerformanceTimer.dumpProfile(globalStart, globalStartNanos)
          if (Config.dumpStats) PerformanceTimer.dumpStats()        
          System.gc()
        }
      }

      //println("Done Executing " + numTimes + " Runs")
      
      if(Config.dumpStats)
        PerformanceTimer.dumpStats()

      executor.shutdown()
    }
    catch {
      case i: InterruptedException => abnormalShutdown(); throw outstandingException //a worker thread threw the original exception        
      case e: Exception => abnormalShutdown(); throw e       
    }
    finally {
      Arguments.args = null
      Arguments.staticDataMap = null
    }
  }

  def loadDeliteDEG(filename: String) = {
    val deg = Path(filename)
    if (!deg.isFile)
      error(filename + " does not exist")
    DeliteTaskGraph(deg.jfile)
  }

  def loadSources(graph: DeliteTaskGraph) {
    for (target <- Targets.values) {
      if (graph.targets contains target)
        Compilers(target).cacheDegSources(Directory(Path(graph.kernelPath + File.separator + Compilers(target).target + File.separator).toAbsolute))
    }
  }

  //abnormal shutdown
  def shutdown(reason: Exception) {
    outstandingException = reason
    mainThread.interrupt()
  }

  // maps op ids to the op's source info (fileName, line, opName)
  // used in the profiler
  var sourceInfo: Map[String, (String, Int, String)] = Map()
}
