package ppl.dsl.optiml.datastruct.scala

import collection.mutable.HashMap
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import scala.collection.mutable.ArrayBuffer

/**
 * Author: Bo Wang
 * Date: May 20, 2011
 * Time: 3:15:46 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Profiler
{
  // "name" -> (chuck_id -> start_time)
  // "x34"  -> (0 -> 12.34)
  val currentParallelTimer = new HashMap[String, HashMap[Int,Long]]

  // "name" -> (size, run_time)
  // "x34"  -> (1000, 0.23)
  val parallelTimer = new HashMap[String, ArrayBuffer[(Int,Double)]]

  // "name" -> start_time
  val currentSequentialTimer = new HashMap[String, Long]

  // "name" -> run_time
  val sequentialTimer = new HashMap[String, Double]

  // "name" -> start_time
  val currentCoreTimer = new HashMap[String,Long]

  // "name" -> (size, run_time)
  val coreTimer = new HashMap[String, ArrayBuffer[(Int,Double)]]

  val opType = new HashMap[String, String]

  val parallelOpLock1: AnyRef = new Object()
  val parallelOpLock2: AnyRef = new Object()
  val sequentialOpLock1: AnyRef = new Object()
  val sequentialOpLock2: AnyRef = new Object()
  val coreOpLock1: AnyRef = new Object()
  val coreOpLock2: AnyRef = new Object()
  
  def recordOpType(kernelId: String, kernelType: String) {
    opType += kernelId -> kernelType
  }

  // Note: the assumption here is the same kernel will not be launched twice at the same time
  //       for example, kernel_x34 is splited into 4 chunks, the identities here are
  //       x34_0, x34_1, x34_2, x34_3. if kernel_x34 is launched twice, then the timing
  //       information is incorrect because of naming conflicts.
  def startParallelOp(kernelId: String, chunkId: Int, printMessage: Boolean = false) {
    parallelOpLock1.synchronized {
      if (!parallelTimer.contains(kernelId)) {
        parallelTimer += kernelId -> new ArrayBuffer[(Int, Double)]
      }
      if (!currentParallelTimer.contains(kernelId)) {
        currentParallelTimer += kernelId -> new HashMap[Int, Long]
      }
      currentParallelTimer(kernelId) += chunkId -> System.nanoTime
      parallelOpLock1.notifyAll
    }
    if (printMessage) println("[PROFILE] " + kernelId + " #" + chunkId + " started")
  }

  def stopParallelOp(kernelId: String, chunkId: Int, kernelSize: Int, printMessage: Boolean = false) {
    val newRecord = (kernelSize, (System.nanoTime - currentParallelTimer(kernelId)(chunkId)).toDouble)
    parallelOpLock2.synchronized {
      parallelTimer(kernelId) += newRecord
      parallelOpLock2.notifyAll
    }
    if (printMessage)
      println("[PROFILE] " + kernelId + " #" + chunkId + " stopped (size = " + kernelSize + ", time = " + newRecord._2 + ")")
  }

  def totalParallelOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- parallelTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + totalTime.formatted("%.0f") + " ns" + appendMessage)
  }

  def printParallelOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- parallelTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + opType(kernelId) + ", total-time(on all cores) = " + totalTime.formatted("%.0f") + " ns")
    for((k,v) <- parallelTimer(kernelId)){
      printf("              size = %d, time = %.0f ns\n", k, v)
    }
  }


  def startSequentialOp(kernelId: String) {
    sequentialOpLock1.synchronized {
      if (!sequentialTimer.contains(kernelId)) {
        sequentialTimer += kernelId -> 0.0
      }
      currentSequentialTimer += kernelId -> System.nanoTime
      sequentialOpLock1.notifyAll
    }
  }

  def stopSequentialOp(kernelId: String) {
    sequentialOpLock2.synchronized {
      sequentialTimer(kernelId) += (System.nanoTime - currentSequentialTimer(kernelId))
      sequentialOpLock2.notifyAll
    }
  }

  def printSequentialOpTime(kernelId: String){
    println("[PROFILE] " + kernelId + ": " + opType(kernelId) + ", total-time = " + sequentialTimer(kernelId).formatted("%.0f") + " ns")
  }


  def startCoreOpTime(kernelId: String, printMessage: Boolean = false) {
    coreOpLock1.synchronized {
      if (!coreTimer.contains(kernelId)) {
        coreTimer += kernelId -> new ArrayBuffer[(Int, Double)]
        opType += kernelId -> "SingleTask"
      }
      currentCoreTimer += kernelId -> System.nanoTime
      coreOpLock1.notifyAll
    }
    if (printMessage)
      println("[PROFILE] " + kernelId + " #" + coreTimer(kernelId).size + " started")
  }

  def stopCoreOpTime(kernelId: String, kernelSize: Int, printMessage: Boolean = false) {
    val newRecord = (kernelSize, (System.nanoTime - currentCoreTimer(kernelId)).toDouble)
    coreOpLock2.synchronized {
      coreTimer(kernelId) += newRecord
      coreOpLock2.notifyAll
    }
    if (printMessage)
      println("[PROFILE] " + kernelId + " #" + coreTimer(kernelId).size + " stopped (size = " + kernelSize + ", time = " + newRecord._2 + ")")
  }

  def totalCoreOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- coreTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + totalTime.formatted("%.0f") + " ns" + appendMessage)
  }

  def printCoreOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- coreTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + opType(kernelId) + ", total-time(on all cores) = " + totalTime.formatted("%.0f") + " ns")
    for((k,v) <- coreTimer(kernelId)){
      printf("              size = %d, time = %.0f ns\n", k, v)
    }
  }

  def printAll() {
    // print out
    for((kernelName, timeList) <- parallelTimer){
      printParallelOpTime(kernelName)
    }

    for((kernelName, time) <- sequentialTimer) {
      printSequentialOpTime(kernelName)
    }

    for((kernelName, timeList) <- coreTimer){
      printCoreOpTime(kernelName)
    }

    // profile
    val parallelModel = LinearRegression.fit(parallelTimer)
    for((kernelName, (a,b,r)) <- parallelModel){
      println("[REGRESSION] " + kernelName + ": " + opType(kernelName) + ", a = " + a + ", b = " + b + ", r = " + r)
    }

    val coreModel = LinearRegression.fit(coreTimer)
    for((kernelName, (a,b,r)) <- coreModel){
      println("[REGRESSION] " + kernelName + ": " + opType(kernelName) + ", a = " + a + ", b = " + b + ", r = " + r)
    }

    // output as JSON
    val stream = new PrintWriter(new FileWriter("out.mod"))
    stream.println("{\"OpModels\":{\n" +
                   "\"ops\": [")
    generateJSON(parallelTimer, parallelModel, stream)
    generateJSON(coreTimer, coreModel, stream)
    stream.println("}\n}")
    stream.flush
  }

  def clearAll() {
    parallelTimer.clear
    sequentialTimer.clear
    coreTimer.clear
  }

  def generateJSON(timer: HashMap[String, ArrayBuffer[(Int,Double)]], model: HashMap[String, (Double,Double,Double)], stream: PrintWriter){

    for((kernelId, timeList) <- timer){
      stream.print("{\"type\": \"" + opType(kernelId) + "\",\n" +
                     "\"kernelId\": \"" + kernelId + "\",\n" +
                     "\"timing\": [\n"
                    )
      for((size, time) <- timeList){
        stream.print("\"" + size + "\": " + time + ",\n")
      }
      val (a,b,r) = model(kernelId)
      stream.print("],\n" +
                     "\"model\": [\n" +
                     "\"type\": \"Linear\",\n" +
                     "\"a\": " + a + ",\n" +
                     "\"b\": " + b + ",\n" +
                     "\"r\": " + r + "\n" + 
                     "]\n")
      stream.print("},\n")
    }
  }

}