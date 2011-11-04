package ppl.apps.collections.linefit



import scala.virtualization.lms.common._
import java.io._
import ppl.delite.framework.collections._
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.{ArraySeqImpl, ArraySeq}



trait LineFit extends CollectionsApplication {
  def main() {
    // load data
    val reader = BufferedReader(FileReader("linefit.txt"))
    val linesbuffer = ArrayBuffer[String]()
    var line = reader.readLine
    while (line != null) {
      linesbuffer += line
      line = reader.readLine
    }
    val points = ArraySeq.fromArrayBuffer(linesbuffer).map {
      l =>
      val xy = l.split(" ")
      (Double.parseDouble(xy(0)), Double.parseDouble(xy(1)))
    }
    
    tic(points)
    // number of points
    val n = points.size
    
    // find x average
    val xavg = points.sumBy(_._1)
    
    // find y average
    val yavg = points.sumBy(_._2)
    
    // find x deviation
    val stt = points.sumBy {
      xy => (xy._1 - xavg) * (xy._1 - xavg)
    }
    
    // find b
    val b = points.sumBy {
      xy => (xy._1 - xavg) * xy._2
    } / stt
    
    // find a
    val a = yavg - xavg * b
    
    // compute chi2
    val chi2 = points.sumBy {
      xy => (xy._2 - a - b * xy._1) * (xy._2 - a - b * xy._1)
    }
    
    // compute siga and sigb
    val siga = Math.sqrt(((unit(1.0) / n) + (xavg * xavg / stt)) * chi2 / n)
    val sigb = Math.sqrt((unit(1.0) / stt) * chi2 / n)
    toc(sigb)
    
    println(a, b, siga, sigb)
  }
}


object LineFitRunner extends CollectionsApplicationRunner with LineFit with StaticDataExp










