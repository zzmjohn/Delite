package ppl.dsl.optigraph.ops
import ppl.dsl.optigraph.EdgeProperty
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}

trait EdgePropertyImplOps{	this: OptiGraph =>
	def edgeprop_defer_impl[A:Manifest](ep: Rep[EdgeProperty[A]], idx: Rep[Int], x: Rep[A]):Rep[Unit]
	def edgeprop_assign_impl[A:Manifest](ep: Rep[EdgeProperty[A]], idx: Rep[Int]):Rep[Unit]
}

trait EdgePropertyImplOpsStandard extends EdgePropertyImplOps{
	this: OptiGraphCompiler with OptiGraphLift =>

	def edgeprop_defer_impl[A:Manifest](ep: Rep[EdgeProperty[A]], idx: Rep[Int], x: Rep[A]) = {
		edgeprop_set_def(ep, idx, x)
		edgeprop_set_is_def(ep, idx)
	}

	def edgeprop_assign_impl[A:Manifest](ep: Rep[EdgeProperty[A]], idx: Rep[Int]) = {
		if(edgeprop_has_def(ep, idx)) {
      		edgeprop_raw_update(ep, idx, edgeprop_get_def(ep, idx))
      		edgeprop_clear_def(ep, idx)
		}
	}

}



  