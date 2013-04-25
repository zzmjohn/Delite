package ppl.dsl.optiml.vector

import scala.lms.ScalaOpsPkg
import scala.lms.{BaseExp, Base}
import ppl.delite.framework.Interfaces
import ppl.dsl.optiml.{OptiMLLift, OptiMLCompiler, OptiML}

trait IndexVectorImplOps { this: OptiML =>
  def index_vector_obj_fromvec_impl(xs: Interface[Vector[Int]]): Rep[IndexVectorDense]
}

trait IndexVectorImplOpsStandard extends IndexVectorImplOps {
  this: OptiMLCompiler with OptiMLLift =>

  def index_vector_obj_fromvec_impl(xs: Interface[Vector[Int]]) = {
    val out = IndexVector(0,xs.isRow) << xs
    out.unsafeImmutable.asInstanceOf[Rep[IndexVectorDense]]
  }

}