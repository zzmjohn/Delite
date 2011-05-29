package ppl.apps.profile

import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

/**
 * Author: Bo Wang
 * Date: May 28, 2011
 * Time: 11:44:19 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object ProfileTest4Runner extends OptiMLApplicationRunner with ProfileTest4

trait ProfileTest4 extends OptiMLApplication {

  def print_usage = {
    println("Usage: ProfileTest4 <m> <n> <num_run>")
    exit(-1)
  }

  def main() = {

    if (args.length < 3) print_usage

    val m = Integer.parseInt(args(0))
    val n = Integer.parseInt(args(1))
    val num_run = Integer.parseInt(args(2))

    val a1 = Matrix.ones(m,n)
    val b1 = Matrix.ones(n,m)
    val a2 = Matrix.ones(m/2,n)
    val b2 = Matrix.ones(n,m)
    val a3 = Matrix.ones(m/2,n)
    val b3 = Matrix.ones(n,m/2)
    val a4 = Matrix.ones(m/2,n/2)
    val b4 = Matrix.ones(n/2,m)
    val a5 = Matrix.ones(m/2,n/2)
    val b5 = Matrix.ones(n/2,m/2)

    var i = 0
    while(i < num_run){
      val c1 = a1 * b1
      println(c1(i,i))
      val c2 = a2 * b2
      println(c2(i,i))
      val c3 = a3 * b3
      println(c3(i,i))
      val c4 = a4 * b4
      println(c4(i,i))
      val c5 = a5 * b5
      println(c5(i,i))
      i += 1
    }

    profilePrintAll()
  }
}



