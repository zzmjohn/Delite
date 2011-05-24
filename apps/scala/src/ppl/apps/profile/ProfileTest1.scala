package ppl.apps.profile

import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

/**
 * Author: Bo Wang
 * Date: May 22, 2011
 * Time: 7:02:38 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object ProfileTest1Runner extends OptiMLApplicationRunner with ProfileTest1

trait ProfileTest1 extends OptiMLApplication {

  def print_usage = {
    println("Usage: ProfileTest1 <vector_size> <num_run>")
    exit(-1)
  }

  def main() = {

    if (args.length < 2) print_usage
    
    val vector_size = Integer.parseInt(args(0))
    val num_run = Integer.parseInt(args(1))

    val a = Vector[Int](vector_size,true)
    var i = 0
    while(i < num_run){
      val b = a map (_+i)
      println(b(i))
      i += 1
    }
    profilePrintAll()

  }
}



