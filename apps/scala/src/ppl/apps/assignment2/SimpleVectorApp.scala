package ppl.apps.assignment2

import ppl.dsl.assignment2.{SimpleVectorApplicationRunner, SimpleVectorApplication}
import ppl.dsl.assignment2.Vector

object SimpleVectorAppRunner extends SimpleVectorApplicationRunner with SimpleVectorApp

trait SimpleVectorApp extends SimpleVectorApplication {

  // I am using the simplest ODE solution, the Euler method
  // http://en.wikipedia.org/wiki/Euler_method
  // paramaters
  //    func(t, y) : the rhs of differential equation y' = func(t,y)
  //    t0         : the starting time
  //    tf         : the ending time
  //    step       : the increment of time
  //    y0         : the initial value of y
  def ode(func:(Rep[Double],Rep[Vector[Double]])=>Rep[Vector[Double]],
          t0:Rep[Double], tf:Rep[Double], step: Rep[Double], y0:Rep[Vector[Double]]) = {

    val num_steps = 100
    // TODO: numeric divide is not supported by LMS
    // val num_steps = ((tf-t0)/step).asInstanceOfL[Int]
    val T = Vector[Double](num_steps)
    val Y = Vector[Vector[Double]](num_steps)
    var t = t0
    var y = y0
    var idx = 0
    while(idx < num_steps){
      T(idx) = t
      Y(idx) = y
      t = t + step
      y = y + func(t,y)*step
      idx += 1
    }
    (T,Y)

  }

  // righd is the differential equation to be solved
  // rigid = y'(t,y) = func(t,y)
  /*
  // example 1
  def rigid(t: Rep[Double], y: Rep[Vector[Double]]) = {
    val dy = Vector[Double](1)
    dy update (0, 3.0*y.apply(0)*t*t-2.0*t)
    dy
  }
  */

  // example 2
  // from Matlab example http://www.mathworks.com/help/techdoc/ref/ode23.html
  def rigid(t: Rep[Double], y: Rep[Vector[Double]]) = {
    val dy = Vector[Double](3)
    dy update (0, y.apply(1)*y.apply(2))
    dy update (1, -1.0*y.apply(0)*y.apply(2))
    dy update (2, -0.51*y.apply(0)*y.apply(1))
    dy
  }

  def main() {

    // testing for part 4
    val x1 = Vector[Int](10) + 1
    val x2 = Vector[Int](10) + 2
    val x3 = 3 * x1
    val x4 = x2 * 4
    println("testing for part 4")
    x3.pprint
    x4.pprint

    // testing for part 5
    val x5 = 5 * x4 + x3
    println("testing for part 5")
    println("note: the AXPY kernel is matched during staging, which leads to printing a [AXPY matched] string")
    x5.pprint

    // testing for part 6
    val x6 = Vector[Int](10)
    x6 += x1
    x6 += x2
    println("testing for part 6")
    x6.pprint

    // testing for part 7
    /*
    // parameters for example 1
    val t0 = 0.0
    val tf = 1.0
    val step  = 0.025
    val y0 = Vector[Double](1)
    y0 update (0, 1.0)
    */

    // parameters for example 2
    val t0 = 0.0
    val tf = 10.0
    val step  = 0.1
    val y0 = Vector[Double](3)
    y0.update(0, 0.0)
    y0.update(1, 1.0)
    y0.update(2, 1.0)

    val (t,y) = ode(rigid, t0, tf, step, y0)

    // print result!
    println("testing for part 7")
    var i = 0
    while(i < y.length){
      print("[ t = " + t.apply(i) + " ]")
      y.apply(i).pprint
      i += 1
    }
  }
}
