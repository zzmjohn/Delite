package ppl.dsl.optiml

import matrix.{ScalaGenMatrixOpsPrfl, MatrixOpsExpPrfl}
import vector.{ScalaGenVectorOpsPrfl, VectorOpsExpPrfl}
import ppl.delite.framework.DeliteApplication

/**
 * Author: Bo Wang
 * Date: May 28, 2011
 * Time: 4:52:30 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */



trait OptiMLPrfl extends MatrixOpsExpPrfl with VectorOpsExpPrfl {
  this: OptiMLExp =>
}

trait OptiMLCodeGenScalaPrfl extends ScalaGenMatrixOpsPrfl with ScalaGenVectorOpsPrfl {
  val IR: DeliteApplication with OptiMLExp
}

