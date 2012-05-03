package ppl.delite.benchmarking.reverseweb



import scala.testing.Benchmark
import collection.mutable.ArrayBuffer
import collection.mutable.ArraySeq
import java.io._
import collection._
import collection.parallel.ForkJoinTasks



object ReverseWeb extends Benchmark {
  
  parallel.tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Config.num_procs)
  
  // load data
  val reader = new BufferedReader(new FileReader("../../dsls/collections/links-sorted-big.txt"))
  val linesbuffer = ArrayBuffer[String]()
  var line = reader.readLine
  while (line != null) {
    linesbuffer += line
    line = reader.readLine
  }
  val pagelinks = linesbuffer.par
  println("Input data loaded")
  
  def run() {
    // flatMap it
    val sourcedests = pagelinks flatMap {
      l =>
      val sourcedests = l.split(":")
      val source = sourcedests(0).toLong
      val dests = sourcedests(1).trim.split(" ")
      dests.map(d => (d, source.toInt))
    }
    
    // groupBy it
    val inverted = sourcedests groupBy {
      x => x._1
    }
  }
  
}
