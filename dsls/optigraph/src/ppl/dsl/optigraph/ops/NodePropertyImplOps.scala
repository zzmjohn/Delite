package ppl.dsl.optigraph.ops
import ppl.dsl.optigraph.NodeProperty
import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}

trait NodePropertyImplOps{	this: OptiGraph =>
	def nodeprop_defer_impl[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int], x: Rep[A]):Rep[Unit]
	def nodeprop_assign_impl[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int]):Rep[Unit]
}

trait NodePropertyImplOpsStandard extends NodePropertyImplOps{
	this: OptiGraphCompiler with OptiGraphLift =>

	def nodeprop_defer_impl[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int], x: Rep[A]) = {
		nodeprop_set_def(np, idx, x)
		nodeprop_set_is_def(np, idx)
	}

	def nodeprop_assign_impl[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int]) = {
		if(nodeprop_has_def(np, idx)) {
      		nodeprop_raw_update(np, idx, nodeprop_get_def(np, idx))
      		nodeprop_clear_def(np, idx)
		}
	}

}



  