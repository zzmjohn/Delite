package ppl.apps.fusion.ml

import scala.virtualization.lms.common._
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.delite.framework.datastruct.scala._

/**
 * Benchmarking of flatMap loop fusion.
 */
object YieldLoopFusionBenchmarkRunner extends OptiMLApplicationRunner with YieldLoopFusionBenchmark

trait YieldLoopFusionBenchmark extends OptiMLApplication {

  def main() {

    // load data from the file
    val lines = Vector[String](2629980, true)
    val file = "/home/vjovanov/work/research/benchmarking-data/california-sun-radiation/all-california.txt"
    val reader = BufferedReader(FileReader(file))
    var line = reader.readLine
    var i = 0
    while (line != null) {
      lines(i) = line.trim
      line = reader.readLine
      i = i + 1
    }

    // measure execution time 
    tic(lines)
    val res = lines.filter(x => {true})
      .flatMap(x => Vector.range(0, 1))
//      .map(_.toInt)
//     .sum
    toc(res)
    
  }

}
