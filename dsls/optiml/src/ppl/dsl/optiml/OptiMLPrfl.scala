package ppl.dsl.optiml

import matrix.{ScalaGenMatrixOpsPrfl, MatrixOpsExpOptPrfl}

/**
 * Author: Bo Wang
 * Date: May 28, 2011
 * Time: 4:52:30 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait OptiMLPrfl extends MatrixOpsExpOptPrfl {
  this: OptiMLExp =>
}

trait OptiMLCodeGenScalaPrfl extends ScalaGenMatrixOpsPrfl

