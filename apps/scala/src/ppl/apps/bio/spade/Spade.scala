package ppl.apps.bio.spade

import ppl.dsl.optiml.datastruct.scala.{Vector,Matrix}
import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

object SpadeRunner extends OptiMLApplicationRunner with Spade

trait Spade extends OptiMLApplication with Downsampling with Upsampling with Clustering {
  def print_usage = {
    println("Usage: Spade <input data file> <output data file>")
    exit(-1)
  }

  def main() = {
    if (args.length < 2) print_usage

    /********* downsampling *********/
    /*
    val data = TrainingSet(readMatrix(args(0)), Labels[Int](0))
    tic()
    val densities = downsample(data, 5., 5., Vector[Int](0), false, 1.)
    toc(densities)
    var i = 0
    while (i < 9){
      print(densities(i) + " ")
      i += 1
    }
    println()
    */

    /********* clustering *********/

    val data = readMatrix(args(0) + "/data.txt")
    tic()
    val assgn = cluster(data, 200)
    toc(assgn)
    for(i <- 0 to 9)
      print(assgn(i)+"\t")
    println()
    
    /*
    val numbers = Vector.rand(10).mutable
    numbers.pprint
    var i = 0
    while(i < 5){
      numbers = numbers.partition(_ > 0.5)._1
      /*
      var j = 0
      while(j < numbers.length){
        numbers(j) = numbers(j) - 0.1
        j += 1
      }
      */
      numbers = numbers map { _ += 0.1 }
      numbers.pprint
      i += 1
    }

    var i = 0
    while(i < 5){
      numbers mfilter {n:Rep[Double]=> n > 0.5}
      var j = 0
      while(j < numbers.length){
        numbers(j) = numbers(j) - 0.1
        j += 1
      }
      i += 1
    }
    */
    /*
    def next(numbers: Rep[Vector[Double]], i: Int):Rep[Vector[Double]] =
    if(i == 5) numbers
    else {
      val new_numbers = numbers.partition(_ > 0.5)._1
      var j = 0
      while(j < new_numbers.length){
        new_numbers(j) = new_numbers(j) - 0.1
        j += 1
      }
      new_numbers.pprint
      next(new_numbers, i+1)
    }
    next(numbers, 0)
    */

    /********* upsampling *********/
    /*
    val tbl = readMatrix(args(0) + "/tbl_small.txt")
    val cluster_data = readMatrix(args(0) + "/cluster_data_small.txt")
    val cluster_assign = readMatrix(args(0) + "/cluster_assign_small.txt").mapRows{row:Rep[Vector[Double]]=> row(0).asInstanceOfL[Int]}

    tic()
    val assign = upsample(tbl, cluster_data, cluster_assign)
    toc(assign)

    for(i <- 0 to 9)
      print(assign(i)+"\t")
    println()
    */
  }

}
