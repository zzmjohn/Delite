package ppl.apps.profile

import ppl.dsl.optiml.{OptiMLApplication, OptiMLApplicationRunner}

/**
 * Author: Bo Wang
 * Date: May 30, 2011
 * Time: 7:38:36 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object ProfileTest5Runner extends OptiMLApplicationRunner with ProfileTest5

trait ProfileTest5 extends OptiMLApplication {

  def print_usage = {
    println("Usage: ProfileTest5 <m> <n>")
    exit(-1)
  }

  def main() = {

    if (args.length < 2) print_usage

    val m = Integer.parseInt(args(0))
    val n = Integer.parseInt(args(1))

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

    val v1 = Vector.ones(m*n)
    val v2 = Vector.ones(m*n/2)
    val v3 = Vector[Double](m,true)
    val v4 = Vector[Double](n,true)

    val c1 = a1 * b1
    val c2 = a2 * b2
    val c3 = a3 * b3
    val c4 = a4 * b4
    val c5 = a5 * b5

    val v5 = v1 map (_*5.0)
    var counter = 0
    v2 foreach {e=> if(e > 5.0) counter+=1 else counter-=1 }
    
    val m7 = v3 ** v4

    println(c1(1,1))
    println(c2(1,1))
    println(c3(1,1))
    println(c4(1,1))
    println(c5(1,1))
    println(v5(1))
    println(counter)
    println(m7(1,1))

    // profilePrintAll()
  }
}


