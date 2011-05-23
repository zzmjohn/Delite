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
    if (printMessage) println("[PROFILE]: Timing " + component + " #" + (times(component).size - 1) + " stopped")
  }

  def totalTime(component: String, appendMessage: String = null) {
    val total = times(component).toList.reduceLeft[Double](_+_)
    println("[PROFILE]: " + component + ": " + total + "s" + appendMessage)
  }

  def clearAll() {
    for((k,v) <- times) {
      v.clear
    }
  }

  def print(component: String) {
    val timeStr = times.get(component) map { "[PROFILE]: Latest time for component " + component + ": " +  _.last.formatted("%.6f") + "s" }
    println(timeStr getOrElse "[PROFILE]: No data for component " + component)
  }

  def printAll() {
    for((k,v) <- times){
      val total = v.toList.reduceLeft[Double](_+_)
      println("[PROFILE]: " + k + ": " + total + "s")
    }
  }

}
