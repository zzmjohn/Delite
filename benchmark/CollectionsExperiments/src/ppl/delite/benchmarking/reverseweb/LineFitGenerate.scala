package ppl.delite.benchmarking.reverseweb



import collection.mutable.ArrayBuffer
import collection.mutable.ArraySeq
import java.io._
import collection._



object LineFitGenerate {
  
  def main(args: Array[String]) {
    val num = args(0).toInt
    val a = 1
    val b = 0
    val fw = new BufferedWriter(new FileWriter("../../linefit.txt"))
    
    for (i <- 0 until num) {
      val x = i * 0.1
      val y = a * x + b
      val xe = x + (math.random - 0.5) * num / 50
      val ye = y + (math.random - 0.5) * num / 50
      fw.write(xe + " " + ye)
      fw.newLine()
    }
    
    fw.close()
  }
  
}
