package ppl.dsl.optiml.datastruct.scala

import collection.mutable.HashMap
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import scala.collection.mutable.ArrayBuffer

object ProfileTimer
{
  val currentTimer = new HashMap[String, Long]
  val times = new HashMap[String, ArrayBuffer[Double]]

  def start(component: String, printMessage: Boolean = true) {
    if (!times.contains(component)) {
      times += component -> new ArrayBuffer[Double]()
    }
    if (printMessage) println("[PROFILE]: Timing " + component + " #" + times(component).size + " started")
    currentTimer += component -> System.currentTimeMillis
  }

  def stop(component: String, printMessage: Boolean = true) {
    val x = (System.currentTimeMillis - currentTimer(component)) / 1000D
    times(component) += x
    /*
    print(component)
    for(i <- 0 until times(component).length)
      printf(" %.6f", times(component)(i))
    println()
    */
    if (printMessage) println("[PROFILE]: Timing " + component + " #" + (times(component).size - 1) + " stopped")
  }

  def totalTime(component: String, appendMessage: String = null) {
    val total = times(component).toList.reduceLeft[Double](_+_)
    println("[PROFILE]: " + component + ": " + total.formatted("%.6f") + "s" + appendMessage)
  }

  def clearAll() {
    for((k,v) <- times) {
      v.clear
    }
  }

  def printTime(component: String, appendMessage: String = null) {
    val timeStr = times.get(component) map { "[PROFILE]: " + component + ": " +  _.last.formatted("%.6f") + "s" + appendMessage}
    println(timeStr getOrElse "[PROFILE]: No data for component " + component)
  }

  def printAll() {
    for((k,v) <- times){
      /*
      print(component)
      for(i <- 0 until v.length)
        printf("%.6f ", v(i))
      println()
      */
      val total = v.toList.reduceLeft[Double](_+_)
      println("[PROFILE-SUMMARY]: " + k + ": " + total.formatted("%.6f") + "s")
    }
  }

}
