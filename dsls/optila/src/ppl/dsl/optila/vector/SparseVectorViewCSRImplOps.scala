package ppl.dsl.optila.vector

import scala.lms.ScalaOpsPkg
import scala.lms.{BaseExp, Base}
import ppl.dsl.optila._

trait SparseVectorViewCSRImplOps extends SparseVectorViewImplOps {
  this: OptiLACompiler with OptiLALift =>

  def sparse_vectorview_apply_impl[A:Manifest](x: Rep[SparseVectorView[A]], n: Rep[Int]): Rep[A] = {
    val idx = n*x.stride + x.start
    val src = sparse_vectorview_source(x)
    src.dcApply(idx)
  }
}