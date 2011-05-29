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

  def recordOpType(kernelId: String, kernelType: String) {
    opType += kernelId -> kernelType
  }

  // Note: the assumption here is the same kernel will not be launched twice at the same time
  //       for example, kernel_x34 is splited into 4 chunks, the identities here are
  //       x34_0, x34_1, x34_2, x34_3. if kernel_x34 is launched twice, then the timing
  //       information is incorrect because of naming conflicts.
  def startParallelOp(kernelId: String, chunkId: Int, printMessage: Boolean = false) {
    if (!parallelTimer.contains(kernelId)) {
      parallelTimer += kernelId -> new ArrayBuffer[(Int, Double)]
    }
    if (printMessage) println("[PROFILE] " + kernelId + " #" + chunkId + " started")
    if (!currentParallelTimer.contains(kernelId)) {
      currentParallelTimer += kernelId -> new HashMap[Int, Long]
    }
    currentParallelTimer(kernelId) += chunkId -> System.currentTimeMillis
  }

  def stopParallelOp(kernelId: String, chunkId: Int, kernelSize: Int, printMessage: Boolean = false) {
    val newRecord = (kernelSize, (System.currentTimeMillis - currentParallelTimer(kernelId)(chunkId)).toDouble)
    parallelTimer(kernelId) += newRecord
    if (printMessage)
      println("[PROFILE] " + kernelId + " #" + chunkId + " stopped (size = " + kernelSize + ", time = " + newRecord._2 + ")")
  }

  def totalParallelOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- parallelTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + totalTime.formatted("%.2f") + "s" + appendMessage)
  }

  def printParallelOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- parallelTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + opType(kernelId) + ", total-time(on all cores) = " + totalTime.formatted("%.2f") + "ms")
    for((k,v) <- parallelTimer(kernelId)){
      printf("              size = %d, time = %.2fs\n", k, v)
    }
  }


  def startSequentialOp(kernelId: String) {
    if (!sequentialTimer.contains(kernelId)) {
      sequentialTimer += kernelId -> 0.0
    }
    currentSequentialTimer += kernelId -> System.currentTimeMillis
  }

  def stopSequentialOp(kernelId: String) {
    sequentialTimer(kernelId) += (System.currentTimeMillis - currentSequentialTimer(kernelId))
  }

  def printSequentialOpTime(kernelId: String){
    println("[PROFILE] " + kernelId + ": " + opType(kernelId) + ", total-time = " + sequentialTimer(kernelId).formatted("%.2f") + "ms")
  }


  def startCoreOpTime(kernelId: String, printMessage: Boolean = false) {
    if (!coreTimer.contains(kernelId)) {
      coreTimer += kernelId -> new ArrayBuffer[(Int, Double)]
      opType += kernelId -> "SingleTask"
    }
    if (printMessage)
      println("[PROFILE] " + kernelId + " #" + coreTimer(kernelId).size + " started")
    currentCoreTimer += kernelId -> System.currentTimeMillis
  }

  def stopCoreOpTime(kernelId: String, kernelSize: Int, printMessage: Boolean = false) {
    val newRecord = (kernelSize, (System.currentTimeMillis - currentCoreTimer(kernelId)).toDouble)
    coreTimer(kernelId) += newRecord
    if (printMessage)
      println("[PROFILE] " + kernelId + " #" + coreTimer(kernelId).size + " stopped (size = " + kernelSize + ", time = " + newRecord._2 + ")")
  }

  def totalCoreOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- coreTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + totalTime.formatted("%.2f") + "ms" + appendMessage)
  }

  def printCoreOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- coreTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + opType(kernelId) + ", total-time(on all cores) = " + totalTime.formatted("%.2f") + "ms")
    for((k,v) <- coreTimer(kernelId)){
      printf("              size = %d, time = %.2fms\n", k, v)
    }
  }

  def printAll() {
    for((kernelName, timeList) <- parallelTimer){
      printParallelOpTime(kernelName)
    }
    val parallelModel = LinearRegression.fit(parallelTimer)
    for((kernelName, (a,b,r)) <- parallelModel){
      println("[REGRESSION] " + kernelName + ": " + opType(kernelName) + ", a = " + a + ", b = " + b + ", r = " + r)
    }

    for((kernelName, time) <- sequentialTimer) {
      printSequentialOpTime(kernelName)
    }

    for((kernelName, timeList) <- coreTimer){
      printCoreOpTime(kernelName)
    }
    val coreModel = LinearRegression.fit(coreTimer)
    for((kernelName, (a,b,r)) <- coreModel){
      println("[REGRESSION] " + kernelName + ": " + opType(kernelName) + ", a = " + a + ", b = " + b + ", r = " + r)
    }
  }

  def clearAll() {
    parallelTimer.clear
    sequentialTimer.clear
    coreTimer.clear
  }

}
