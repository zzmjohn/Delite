package epfl.mdarrays.tests.pde1

import epfl.mdarrays.library.scala.Conversions._
import epfl.mdarrays.library.scala._

import epfl.mdarrays.staged._
import scala.util.Random

object PDE1BenchmarkStagedRunner extends StagedSACApplicationRunner with PDE1BenchmarkStaged
trait PDE1BenchmarkStaged extends StagedSACApplication with PDE1BenchmarkStagedFakeReadMDArray {

  type MDArrayBool = Rep[MDArray[Boolean]]
  type MDArrayDbl  = Rep[MDArray[Double]]
  type MDArrayInt  = Rep[MDArray[Int]]
  type Dbl         = Rep[MDArray[Double]]

  // modify this to run other tests
  val range = range1 _

  def main() {
    /*
     * Right now we don't have IO from files => let's generate the array by hand
     */
    val matrix = readMDArray[Double]("mdarray-pde1-in.txt")
    startTimer(matrix::Nil)
    val result = range(matrix, 1)
    stopTimer(result::Nil)
    writeMDArray("mdarray-pde1-out.txt", result)
    print(result)
  }

  def testWithLoopExtraction(matrix: MDArrayInt): MDArrayInt = {
    With(function = iv => sel(iv, matrix * 3 - matrix)).GenArray(shape(matrix))
  }

  def range1(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax1, iterations)
  def range2(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax2, iterations)
  def range3(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax3, iterations)
  //def range4(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax4, iterations)
  def range5(matrix: MDArrayDbl, iterations: Int): MDArrayDbl = PDE1impl(matrix, Relax5, iterations)

  def vectorTest() = {
    val v1: Rep[MDArray[Int]] = 1::0::0::Nil
    val v2: Rep[MDArray[Int]] = 0::1::0::Nil
    val v3: Rep[MDArray[Int]] = 0::0::1::Nil

    reshape(1::1::3::Nil, v1 + v2 + v3)
  }

  // The PDE1BenchmarkStaged implementation
  def PDE1impl(matrix: MDArrayDbl,
               Relax: (MDArrayDbl, MDArrayDbl, Dbl) => MDArrayDbl,
               iterations: Int): MDArrayDbl = {

    val red: MDArrayBool = With(lb = List(1, 0, 0), step = List(2,1,1), function = iv => true).
      GenArray(shape(matrix))

    var u = matrix
    val f = matrix
    // Luckily "iterations" is not staged :)
    for (i <- Stream.range(0, iterations)) {
      u = where(red, Relax(u, f, 1d/10d), u)
      u = where(!red, Relax(u, f, 1d/10d), u)
    }

    u
  }

  // The 5 'relax' methods
  def Relax1(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d

    With(lbStrict=true, ubStrict=true, function = iv => {
      // TODO: Fix this forced conversion
      // Here there's a priority problem: the String.+ has more priority than the infix_+ of Rep[MDArray[Int]] so the
      // conversion goes from Rep[MDArray[Int]] to String instead of from List[Int] to Rep[MDArray[Int]]
      val local_sum = u(iv + convertFromListRep(List(1, 0, 0))) + u(iv - convertFromListRep(List(1, 0, 0))) +
                      u(iv + convertFromListRep(List(0, 1, 0))) + u(iv - convertFromListRep(List(0, 1, 0))) +
                      u(iv + convertFromListRep(List(0, 0, 1))) + u(iv - convertFromListRep(List(0, 0, 1)))
      // TODO: Import double operations!
      //factor * (hsq * f(iv) + local_sum)
      (f(iv) * hsq + local_sum) * factor
    }).ModArray(u)
  }

  def Relax2(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    With(lbStrict=true, ubStrict=true, function = iv => {
      val block = tile(shape(W), iv-1, u)
      val local_sum = sum(W * block)
      // TODO: Import double operations!
      //factor * (hsq * f(iv) + local_sum)
      (f(iv) * hsq + local_sum) * factor
    }).ModArray(u)
  }

  def CombineInnerOuter(inner: MDArrayDbl, outer: MDArrayDbl) =
    With(lbStrict=true, ubStrict=true, function = iv => inner(iv)).ModArray(outer)

  def Relax3(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d
    val u1 = f * hsq
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    val u2 = u1 + With(lbStrict=true, ubStrict=true, function = iv => {
      sum(W * tile(shape(W), iv-1, u))
    }).ModArray(u)
    CombineInnerOuter(u2 * factor, u)
  }

//  def Relax4(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {
//
//    val factor:Double = 1d/6d
//    var u1 = f * hsq
//
//    def justOne(size: Int, dim: Int, v: Int): MDArrayInt = {
//      val array = new Array[Int](size)
//      array(dim) = v
//      array
//    }
//
//    for (i <- List.range(0, dim(u))) {
//      u1 = u1 + shift(justOne(u.dim, i, 1), 0d, u)
//      u1 = u1 + shift(justOne(u.dim, i, -1), 0d, u)
//    }
//
//    CombineInnerOuter(u1 * factor, u)
//  }

  def Relax5(u: MDArrayDbl, f: MDArrayDbl, hsq: Dbl): MDArrayDbl = {

    val factor:Double = 1d/6d
    val W = reshape(3::3::3::Nil, (0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil):::(0d::1d::0d::1d::0d::1d::0d::1d::0d::Nil):::(0d::0d::0d::0d::1d::0d::0d::0d::0d::Nil))

    def justOne(size: Int, dim: Int, v: Int): MDArrayInt = {
      val array = new Array[Int](size)
      array(dim) = v
      array
    }

    val u1: MDArrayDbl = With[Double](lb=shape(W) * 0, ub=shape(W)-1, function = iv => shift(-iv + 1, 0d, u)).Fold((a:MDArrayDbl, b:MDArrayDbl) => infix_+(a, b), f * hsq)

    CombineInnerOuter(u1 * factor, u)
  }
}

trait PDE1BenchmarkStagedFakeReadMDArray {
  def fakeReadMDArray(): MDArray[Double] = {
    Random.setSeed(0);

    import epfl.mdarrays.library.scala.Operations._
    import epfl.mdarrays.library.scala.Conversions._
    import epfl.mdarrays.library.scala._

    val size: Int = 64
    val arr: Array[Double] = new Array[Double](size * size * size)
    val rnd: Random = new Random(1) // We need to have a fixed seed
    for (i <- arr.indices)
      arr(i) = rnd.nextDouble()
    val matrix: MDArray[Double] = reshape(size :: size :: size :: Nil, arr)
    matrix
  }
}
