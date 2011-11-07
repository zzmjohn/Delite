package ppl.apps.fusion.ml

import scala.virtualization.lms.common._
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}
import ppl.delite.framework.datastruct.scala._
import ppl.dsl.optiml.datastruct.scala.Vector

/**
 * Benchmarking of flatMap loop fusion.
 */
object YieldLoopFusionBenchmarkRunner extends OptiMLApplicationRunner with YieldLoopFusionBenchmark

trait YieldLoopFusionBenchmark extends OptiMLApplication {
  
  def main() {
    // load data from the file
    val userName = "John"
    val file = "/home/vojin/benchmarking-data/twitter-tweets-processed/dataset-60000"
    val reader = BufferedReader(FileReader(file))
    var line = reader.readLine.trim
    var users = line.toInt
    println(users)
    val friends = Vector[(String, Vector[String], String)](users, true)


    var i = 0
    while (i < users ) {
      line = reader.readLine
      val values = line.split("\\\\|header-delimiter\\\\|")
      val user = values(0)
      val messages = values(1).toInt
      val status = values(2)
      val tweets = Vector[String](messages, true)
      var j = 0
      while (j < messages) {
	line = reader.readLine 
        val tweetLine = line.split("\\\\|tweet-delimiter\\\\|")
	val time = tweetLine(0).toInt
	vector_update(tweets, j, tweetLine(1))
	j = j + 1 
      }
      vector_update(friends, i, (user, tweets, status))
      i = i + 1
    }


    // measure execution time 
    tic(friends)
    val res = friends.filter(x => x._3 == "online").flatMap(x => x._2).filter(v => v != userName)

    toc(res)
  }
}
