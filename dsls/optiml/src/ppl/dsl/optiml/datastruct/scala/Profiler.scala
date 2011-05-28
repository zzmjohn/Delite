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
  val currentParallelTimer = new HashMap[String, HashMap[Int, Double]]

  // "name" -> (size_id, run_time)
  // "x34"  -> (1000, 0.23)
  val parallelTimer = new HashMap[String, ArrayBuffer[(Int,Double)]]

  val currentSequentialTimer = new HashMap[String, Double]

  val sequentialTimer = new HashMap[String, Double]

  val coreKernelTimer = new HashMap[String, Double]

  val opType = new HashMap[String, String]

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
      currentParallelTimer += kernelId -> new HashMap[Int, Double]
    }
    currentParallelTimer(kernelId) += chunkId -> System.currentTimeMillis
  }

  def stopParallelOp(kernelId: String, chunkId: Int, kernelSize: Int, printMessage: Boolean = false) {
    val newRecord = (kernelSize, (System.currentTimeMillis - currentParallelTimer(kernelId)(chunkId)) / 1000D)
    parallelTimer(kernelId) += newRecord
    if (printMessage)
      println("[PROFILE] " + kernelId + " #" + chunkId + " stopped (size = " + kernelSize + ", time = " + newRecord._2 + ")")
  }

  def totalParallelOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- parallelTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + totalTime.formatted("%.6f") + "s" + appendMessage)
  }

  def printParallelOpTime(kernelId: String, appendMessage: String = null) {
    var totalTime = 0.0
    for((k,v) <- parallelTimer(kernelId)){
      totalTime += v
    }
    println("[PROFILE] " + kernelId + ": " + opType(kernelId) + ", total-time(on all cores) = " + totalTime.formatted("%.6f") + "s")
    for((k,v) <- parallelTimer(kernelId)){
      printf("              size = %d, time = %.6fs\n", k, v)
    }
  }

  def recordOpType(kernelId: String, kernelType: String) {
    opType += kernelId -> kernelType
  }


  def startSequentialOp(kernelId: String) {
    if (!sequentialTimer.contains(kernelId)) {
      sequentialTimer += kernelId -> 0.0
    }
    currentSequentialTimer += kernelId -> System.currentTimeMillis
  }

  def stopSequentialOp(kernelId: String) {
    sequentialTimer(kernelId) += (System.currentTimeMillis - currentSequentialTimer(kernelId)) / 1000D
  }

  def printSequentialOpTime(kernelId: String){
    println("[PROFILE] " + kernelId + ": " + opType(kernelId) + ", total-time = " + sequentialTimer(kernelId).formatted("%.6f") + "s")
  }


/*
  def startCoreKernel(kernelId: String, printMessage: Boolean = false) {
    if (!coreKernelTimer.contains(kernelId)) {
      coreKernelTimer += kernelId -> new ArrayBuffer[Double]()
    }
    if (printMessage) println("[PROFILE]: " + kernelId + " #" + coreKernelTimer(kernelId).size + " started")
    currentTimer += kernelId -> System.currentTimeMillis
  }

  def stopCoreKernel(kernelId: String, printMessage: Boolean = false) {
    val x = (System.currentTimeMillis - currentTimer(kernelId)) / 1000D
    coreKernelTimer(kernelId) += x
    if (printMessage) println("[PROFILE]: " + kernelId + " #" + (coreKernelTimer(kernelId).size - 1) + " stopped")
  }
*/

  def printAll() {
    for((kernelName, timeList) <- parallelTimer){
      printParallelOpTime(kernelName)
    }

    for((kernelName, time) <- sequentialTimer) {
      printSequentialOpTime(kernelName)
    }

  }

  def clearAll() {
    parallelTimer.clear
    sequentialTimer.clear
    coreKernelTimer.clear
  }

}
