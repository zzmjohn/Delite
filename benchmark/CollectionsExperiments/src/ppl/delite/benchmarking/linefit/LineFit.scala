package ppl.delite.benchmarking.linefit



import scala.testing.Benchmark
import collection.mutable.ArrayBuffer
import collection.mutable.ArraySeq
import java.io._
import collection._
import collection.parallel.ForkJoinTasks



object Config {
  
  val num_procs = Integer.parseInt(System.getProperty("threads", "1"))
  
}


object LineFit extends Benchmark {
  
  parallel.tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Config.num_procs)
  
  // load data
  val reader = new BufferedReader(new FileReader("../../linefit.txt"))
  var linesbuffer = ArrayBuffer[String]()
  var line = reader.readLine
  while (line != null) {
    linesbuffer += line
    line = reader.readLine
  }
  val points = (ArraySeq(linesbuffer: _*).map {
    l =>
    val xy = l.split(" ")
    (xy(0).toDouble, xy(1).toDouble)
  }).par
  linesbuffer = null
  println("data loaded - " + points.size)
  
  def run() {
    linefit[Double](points)
  }
  
  def linefit[T: Manifest: Fractional](points: collection.parallel.mutable.ParArray[(T, T)]) {
    val fr = implicitly[Fractional[T]]
    import fr._
    
    val n = points.size
    
    val xavg = points.aggregate(zero)({
      case (acc, (x, y)) => acc + x
    }, _ + _)
    
    val yavg = points.aggregate(zero)({
      case (acc, (x, y)) => acc + y
    }, _ + _)
    
    val stt = points.aggregate(zero)({
      case (acc, (x, y)) => acc + ((x - xavg) * (x - xavg))
    }, _ + _)
    
    val b = points.aggregate(zero)({
      case (acc, (x, y)) => acc + ((x - xavg) * y)
    }, _ + _) / stt
    
    val a = yavg - (xavg * b)
    
    val chi2 = points.aggregate(zero)({
      case (acc, (x, y)) => (y - a - b * x) * (y - a - b * x)
    }, _ + _)
  }
  
}
