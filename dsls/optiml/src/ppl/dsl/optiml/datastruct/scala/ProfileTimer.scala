package ppl.dsl.optiml.datastruct.scala

import collection.mutable.HashMap
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import scala.collection.mutable.ArrayBuffer

object ProfileTimer
{
  val currentTimer    = new HashMap[String, Long]
  val parallelTimer   = new HashMap[String, ArrayBuffer[Double]]
  val kernelTimer     = new HashMap[String, Double]
  val coreKernelTimer = new HashMap[String, Double]



  def startParallelOP(component: String, printMessage: Boolean = false) {
    if (!parallelTimer.contains(component)) {
      parallelTimer += component -> new ArrayBuffer[Double]()
    }
    if (printMessage) println("[PROFILE]: " + component + " #" + parallelTimer(component).size + " started")
    currentTimer += component -> System.currentTimeMillis
  }

  def stopParallelOP(component: String, printMessage: Boolean = false) {
    val x = (System.currentTimeMillis - currentTimer(component)) / 1000D
    parallelTimer(component) += x
    if (printMessage) println("[PROFILE]: " + component + " #" + (parallelTimer(component).size - 1) + " stopped")
  }

  def totalParallelOPTime(component: String, appendMessage: String = null) {
    val total = parallelTimer(component).toList.reduceLeft[Double](_+_)
    println("[PROFILE]: " + component + ": " + total.formatted("%.6f") + "s" + appendMessage)
  }

  def printParallelOPTime(component: String, appendMessage: String = null) {
    val timeStr = parallelTimer.get(component) map { "[PROFILE]: " + component + ": " +  _.last.formatted("%.6f") + "s" + appendMessage}
    println(timeStr getOrElse "[PROFILE]: No data for component " + component)
  }



  def startKernel(component: String) {
    if (!kernelTimer.contains(component)) {
      kernelTimer += component -> 0.0
    }
    currentTimer += component -> System.currentTimeMillis
  }

  def stopKernel(component: String) {
    kernelTimer(component) += (System.currentTimeMillis - currentTimer(component)) / 1000D
  }

  def printKernelTime(component: String){
    println("[PROFILE]: " + component + ": " + kernelTimer(component).formatted("%.6f") + "s")
  }


/*
  def startCoreKernel(component: String, printMessage: Boolean = false) {
    if (!coreKernelTimer.contains(component)) {
      coreKernelTimer += component -> new ArrayBuffer[Double]()
    }
    if (printMessage) println("[PROFILE]: " + component + " #" + coreKernelTimer(component).size + " started")
    currentTimer += component -> System.currentTimeMillis
  }

  def stopCoreKernel(component: String, printMessage: Boolean = false) {
    val x = (System.currentTimeMillis - currentTimer(component)) / 1000D
    coreKernelTimer(component) += x
    if (printMessage) println("[PROFILE]: " + component + " #" + (coreKernelTimer(component).size - 1) + " stopped")
  }
*/

  def printAll() {
    for((k,v) <- parallelTimer){
      /*
      print(component)
      for(i <- 0 until v.length)
        printf("%.6f ", v(i))
      println()
      */
      val total = v.toList.reduceLeft[Double](_+_)
      println("[PROFILE-SUMMARY]: " + k + ": " + total.formatted("%.6f") + "s")
    }
    for((k,v) <- kernelTimer) {
      println("[PROFILE-SUMMARY]: " + k + ": " + v)
    }
  }

  def clearAll() {
    parallelTimer.clear
    kernelTimer.clear
    coreKernelTimer.clear
  }

}
