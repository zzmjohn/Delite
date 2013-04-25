package ppl.dsl.optila.vector

import scala.lms._
import ppl.dsl.optila._

trait SparseVectorViewImplOps {
  this: OptiLACompiler with OptiLALift =>

  def sparse_vectorview_apply_impl[A:Manifest](x: Rep[SparseVectorView[A]], n: Rep[Int]): Rep[A] 
}