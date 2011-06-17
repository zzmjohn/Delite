package ppl.apps.profile

import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

/**
 * Author: Bo Wang
 * Date: May 23, 2011
 * Time: 1:14:40 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object ProfileTest2Runner extends OptiMLApplicationRunner with ProfileTest2

trait ProfileTest2 extends OptiMLApplication {

  def print_usage = {
    println("Usage: ProfileTest2 <m> <n> <num_run>")
    exit(-1)
  }

  def main() = {

    if (args.length < 3) print_usage
    
    val m = Integer.parseInt(args(0))
    val n = Integer.parseInt(args(1))
    val num_run = Integer.parseInt(args(2))

    val a = Matrix.ones(m,n)
    val b = Matrix.ones(n,m)
    val af = Matrix.onesf(m,n)
    val bf = Matrix.onesf(n,m)

    var i = 0
    while(i < num_run){
      val c = a * b
      println(c(i,i))
      val cf = af * bf
      println(cf(i,i))
      i += 1
    }

    // profilePrintAll()
  }
}



