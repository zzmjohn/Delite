package ppl.apps.profile

import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

/**
 * Author: Bo Wang
 * Date: May 23, 2011
 * Time: 1:39:50 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object ProfileTest3Runner extends OptiMLApplicationRunner with ProfileTest3

trait ProfileTest3 extends OptiMLApplication {

  def print_usage = {
    println("Usage: ProfileTest3 <m> <n> <num_run>")
    exit(-1)
  }

  def main() = {

    if (args.length < 3) print_usage

    val m = Integer.parseInt(args(0))
    val n = Integer.parseInt(args(1))
    val num_run = Integer.parseInt(args(2))

    val a = Matrix.ones(m,n)
    val b = Matrix.ones(n,m)

    var i = 0
    while(i < num_run){
      val c = a + b 
      println(c(i,i))
      i += 1
    }

    // profilePrintAll()
  }
}
