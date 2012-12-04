package ppl.tests.scalatest.dsl.optiml

import ppl.delite.framework.DeliteApplication
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._

/* Testing complex number functionality
 *
 * author:  Arvind Sujeeth (asujeeth@stanford.edu)
 * created: 12/3/12
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object ComplexMathRunner extends DeliteTestRunner with OptiMLApplicationRunner with ComplexMath
trait ComplexMath extends DeliteTestModule with OptiMLApplication {
  def main() = {
    val a = Complex(5,2) 
    val b = Complex(3,-11) 
     
    val c1 = a+b
    collect(c1 Eq Complex(8,-9))
    val c2 = a-b
    collect(c2 Eq Complex(2,13))
    val c3 = a*b
    collect(c3 Eq Complex(37,-49))
    val c4 = a/b
    collect(abs(c4.real)-0.053846 < .01)
    collect(abs(c4.imag)-0.469231 < .01)
    val c5 = a.exp
    collect(abs(c5.real)-61.761667 < .01)
    collect(abs(c5.imag)-134.951704 < .01)    
    val c6 = a.abs
    collect(abs(c6.real)-5.385165 < .01)
    val c7 = a.conj
    collect(c7 Eq Complex(5,-2))
    
    val m = Matrix(Vector(Complex(3,1),Complex(5,0),Complex(0,-2)),Vector(Complex(2,-2),Complex(0,1),Complex(-7,-13)))
    val mT = m.t.map(_.conj) 
    val mRef = Matrix(Vector(Complex(3,-1),Complex(2,2)), Vector(Complex(5,0), Complex(0,-1)), Vector(Complex(0,2), Complex(-7,13)))
    // no object equality with this record implementation
    // collect(mT == mRef)
    m.zip(mT) { (a,b) => a Eq b }
    // collect(mT(0,0) Eq Complex(3,-1))
    // collect(mT(0,1) Eq Complex(2,2))
    // collect(mT(1,0) Eq Complex(5,0))
    // collect(mT(1,1) Eq Complex(0,-1))
    // collect(mT(2,0) Eq Complex(0,2))
    // collect(mT(2,1) Eq Complex(-7,13))

    mkReport
  }
}

class ComplexSuite extends DeliteSuite {
  def testComplexMath() { compileAndTest(ComplexMathRunner) }
}