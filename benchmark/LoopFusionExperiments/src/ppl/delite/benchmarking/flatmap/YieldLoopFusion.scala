package ppl.delite.benchmarking.flatmap

import java.io._
import collection.parallel.ForkJoinTasks
import collection._
/**
 * Benchmarking of flatMap loop fusion.
 */

object YieldLoopFusionBenchmark extends scala.testing.Benchmark {
  
  parallel.tasksupport.asInstanceOf[ForkJoinTasks].forkJoinPool.setParallelism(Config.num_procs)
    // load data from the file
 val userName = "John"
 val file = "/home/vojin/benchmarking-data/twitter-tweets-processed/dataset-60000"
    val reader = new BufferedReader(new FileReader(file))
    var line = reader.readLine.trim
    var users = line.toInt
    println(users)
    val friends = new collection.mutable.ArraySeq[(String, scala.collection.parallel.mutable.ParArray[String], String)](users).par

    var i = 0
    while (i < users ) {
      line = reader.readLine
      val values = line.split("\\|header-delimiter\\|")
      val user = values(0)
      val messages = values(1).toInt
      val status = values(2)
      val tweets = new collection.mutable.ArraySeq[String](messages).par
      var j = 0
      while (j < messages) {
        line = reader.readLine
        val tweetLine = line.split("\\|tweet-delimiter\\|")
        val time = tweetLine(0).toInt
        tweets(j) = tweetLine(1)
        j = j + 1
      }
      friends(i) = (user, tweets, status)
      i = i + 1
    }


  def run() {
    // measure execution time 
    val res = friends.filter(x => x._3 == "online").flatMap(x => x._2).filter(v => v != userName)
  }
}
