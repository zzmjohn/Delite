package epfl.mdarrays.tests.others

import epfl.mdarrays.datastruct.scala.Conversions._
import epfl.mdarrays.datastruct.scala._
import epfl.mdarrays.staged._
import _root_.scala.virtualization.lms.common._
import java.io.{FileWriter, PrintWriter}


class ScopeTestStaged() { this: MDArrayBaseExp with IfThenElseExp =>

  /**
   * This is used to test the == correctness
   */
  def vectorTest() = {
    var arrays: List[MDArray[_]] = Nil

    arrays = (1::0::0::Nil) :: arrays
    arrays = (1::0::0::Nil) :: arrays
    arrays = (0::1::0::Nil) :: arrays
    arrays = (0::0::1::Nil) :: arrays
    arrays = (1.0::0.0::0.0::Nil) :: arrays
    arrays = (true::true::Nil) :: arrays
    arrays = (true::true::Nil) :: arrays
    arrays = reshape ((1::2::Nil), (1::0::Nil)) :: arrays
    arrays = reshape ((1::2::Nil), (1::0::Nil)) :: arrays
    arrays = reshape ((1::2::Nil), (1.0::0.0::Nil)) :: arrays
    arrays = reshape ((2::2::Nil), (1::0::1::0::Nil)) :: arrays
    arrays = reshape ((1::2::Nil), (true::false::Nil)) :: arrays
    arrays = 7 :: arrays
    arrays = 7 :: arrays
    arrays = 7.0 :: arrays
    arrays = 7.1 :: arrays
    arrays = true :: arrays
    arrays = arrays.reverse

    for(i <- Stream.range(0, arrays.length - 1))
      println("v" + i + "= " + arrays(i))

    for(i <- Stream.range(0, arrays.length - 1)) {
      for (j <- Stream.range(i+1, arrays.length - 1))
        println("test v" + i + " vs v" + j + ": " + (arrays(j) == arrays(i)) + (if ((arrays(i) == arrays(j)) != (arrays(j) == arrays(i))) "ASYMETRY DETECTED!" else ""))
      println("---")
    }
  }

  def testStaged(a: Rep[MDArray[Boolean]], b: Rep[MDArray[Int]]): Rep[MDArray[Int]] = {

    // Without scopes, this should fail shape inference
    val constraints =
      if (sel(0::Nil, a)) {
        val add: Rep[MDArray[Int]] = reshape(1::3::Nil, 1::2::3::Nil)
        sel(0::Nil, (b + add))
      } else {
        val add: Rep[MDArray[Int]] = reshape(1::1::Nil, 1::Nil)
        sel(0::Nil, (b + add))
      }

    // With reconciliation, this should be specialized
    sel(0::Nil, With(function = iv => 1).ModArray(b)) + constraints
  }

  def testShapes(a: Rep[MDArray[Int]], b: Rep[MDArray[Int]]) = a + b
}
