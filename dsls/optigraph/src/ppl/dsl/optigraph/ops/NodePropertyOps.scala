package ppl.dsl.optigraph.ops

import reflect.{Manifest, SourceContext}
import ppl.delite.framework.ops._
import scala.virtualization.lms.common.{VariablesExp, Variables}
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.dsl.optigraph._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericFatCodegen}
import reflect.{Manifest, SourceContext}
import java.io.PrintWriter

trait NodePropertyOps extends Variables {
  this: OptiGraph =>

  /** NodeProperty constructors */ 
  object NodeProperty {
    /** Create a new node property (each node's value is set to default of the type) */
    def apply[A:Manifest](g: Rep[Graph]) = nodeprop_new(g)
    /** Create a new node property (each node's value is set to init) */
    def apply[A:Manifest](g: Rep[Graph], init:Rep[A]) = nodeprop_new(g, init)
  }
  /** Alias for NodeProperty */
  object NP {
    def apply[A:Manifest](g: Rep[Graph]) = NodeProperty.apply(g)
    def apply[A:Manifest](g: Rep[Graph], init:Rep[A]) = NodeProperty.apply(g, init)
  }
  
  implicit def repNodePropertyToNodePropertyOps[A:Manifest](np: Rep[NodeProperty[A]]) = new NodePropertyOpsCls(np)
  
  /** Operations on NodeProperties */
  class NodePropertyOpsCls[A:Manifest](np: Rep[NodeProperty[A]]) {
    /** Return the property value of node n */
    def apply(n: Rep[Node]) = nodeprop_apply(np, n)
    /** Update the property value of node n to x */
    def update(n: Rep[Node], x: Rep[A]) = nodeprop_update(np, n, x)
    /** Set the value of all the nodes to x (parallel operation) */
    def setAll(x: Rep[A]) = nodeprop_setAll(np, x)
    /** Defer assigning value x to node n (any other previously deferred value will be overwritten) */
    def <= (n: Rep[Node], x: Rep[A]) = nodeprop_defer(np, n, x)
    /** Assign the value deferred for node n (the latest deferred value) */
    def assign(n: Rep[Node]) = nodeprop_assign(np, n)
    /** Assign the values deferred for all the nodes (parallel operation) */
    def assignAll() = nodeprop_assignAll(np)
  }
  
  def nodeprop_new[A:Manifest](g: Rep[Graph]): Rep[NodeProperty[A]]
  def nodeprop_new[A:Manifest](g: Rep[Graph], init:Rep[A]): Rep[NodeProperty[A]]
  def nodeprop_apply[A:Manifest](np: Rep[NodeProperty[A]], n: Rep[Node]): Rep[A]
  def nodeprop_update[A:Manifest](np: Rep[NodeProperty[A]], n: Rep[Node], x: Rep[A]): Rep[Unit]
  def nodeprop_setAll[A:Manifest](np: Rep[NodeProperty[A]], x: Rep[A]): Rep[Unit]
  def nodeprop_defer[A:Manifest](np: Rep[NodeProperty[A]], n: Rep[Node], x: Rep[A]): Rep[Unit]
  def nodeprop_assign[A:Manifest](np: Rep[NodeProperty[A]], n: Rep[Node]): Rep[Unit]
  def nodeprop_assignAll[A:Manifest](np: Rep[NodeProperty[A]]): Rep[Unit]
  def nodeprop_size[A:Manifest](np: Rep[NodeProperty[A]]):Rep[Int]

  def nodeprop_get_def[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int]):Rep[A]
  def nodeprop_set_def[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int], x: Rep[A]):Rep[Unit]
  def nodeprop_has_def[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int]):Rep[Boolean]
  def nodeprop_set_is_def[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int]):Rep[Unit]
  def nodeprop_clear_def[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int]):Rep[Unit]
  def nodeprop_raw_apply[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int]): Rep[A]
  def nodeprop_raw_update[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int], x: Rep[A]): Rep[Unit]
}

trait NodePropertyOpsExp extends NodePropertyOps with VariablesExp with BaseFatExp {
  this: OptiGraphExp =>
    
  case class NodePropObjectNew[A](g: Exp[Graph], size: Exp[Int]) (val mNP: Manifest[NodeProperty[A]]) extends Def[NodeProperty[A]]

  case class NodePropApply[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) extends Def[A]
  case class NodePropUpdate[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int], x: Exp[A]) extends Def[Unit]

  case class NodePropSetAll[A:Manifest](in: Exp[NodeProperty[A]], x: Exp[A]) extends DeliteOpIndexedLoop {
    val size = nodeprop_size(in)
    def func = i => nodeprop_raw_update(in, i, x)
  }

  case class NodePropDefer[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int], x: Exp[A])
    extends DeliteOpSingleWithManifest[A, Unit](reifyEffectsHere(nodeprop_defer_impl(np, idx, x)))

  case class NodePropGetDef[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) extends Def[A]
  case class NodePropSetDef[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int], x: Exp[A]) extends Def[Unit]

  case class NodePropHasDef[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) extends Def[Boolean]
  case class NodePropSetIsDef[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) extends Def[Unit]
  case class NodePropClearDef[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) extends Def[Unit]

  case class NodePropAssign[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int])
     extends DeliteOpSingleWithManifest[A, Unit](reifyEffectsHere(nodeprop_assign_impl(np, idx)))

  case class NodePropAssignAll[A:Manifest](in: Exp[NodeProperty[A]]) extends DeliteOpIndexedLoop {
    val size = nodeprop_size(in)
    def func = i => {
      if(nodeprop_has_def(in, i)) {
          nodeprop_raw_update(in, i, nodeprop_get_def(in, i))
          nodeprop_clear_def(in, i)
      } else {
        unit()
      }
    }
  }

  case class NodePropSize[A:Manifest](np:Exp[NodeProperty[A]]) extends Def[Int]
  
  // default constructor
  def nodeprop_new[A:Manifest](g: Exp[Graph]) = {
    val size = g.NumNodes
    reflectMutable(NodePropObjectNew(g, size)(manifest[NodeProperty[A]]))
  } 
  // constructor with initial value
  def nodeprop_new[A:Manifest](g: Exp[Graph], init: Exp[A]) = {
    val newNP = NodeProperty(g)
    newNP.setAll(init)
    newNP
  } 
  
  def nodeprop_setAll[A:Manifest](np: Exp[NodeProperty[A]], init: Exp[A]) = reflectWrite(np)(NodePropSetAll(np, init)) 
  
  def nodeprop_apply[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node]) = {
    nodeprop_raw_apply(np,n.Id)
  }
  def nodeprop_update[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node], x: Exp[A]) = {
    nodeprop_raw_update(np,n.Id,x)
  }

  def nodeprop_defer[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node], x: Exp[A]) = reflectWrite(np)(NodePropDefer(np, n.Id, x))

  def nodeprop_get_def[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) = reflectPure(NodePropGetDef(np, idx))
  def nodeprop_set_def[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int], x: Exp[A]) = reflectWrite(np)(NodePropSetDef(np, idx, x))

  def nodeprop_has_def[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) = reflectPure(NodePropHasDef(np, idx))
  def nodeprop_set_is_def[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) = reflectWrite(np)(NodePropSetIsDef(np, idx))
  def nodeprop_clear_def[A:Manifest](np: Exp[NodeProperty[A]], idx: Exp[Int]) = reflectWrite(np)(NodePropClearDef(np, idx))

  def nodeprop_assign[A:Manifest](np: Exp[NodeProperty[A]], n: Exp[Node]) = reflectWrite(np)(NodePropAssign(np,n.Id))
  def nodeprop_assignAll[A:Manifest](np: Exp[NodeProperty[A]]) = reflectWrite(np)(NodePropAssignAll(np))

  def nodeprop_raw_apply[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int]): Rep[A] = reflectPure(NodePropApply(np, idx))
  def nodeprop_raw_update[A:Manifest](np: Rep[NodeProperty[A]], idx: Rep[Int], x: Rep[A]): Rep[Unit] = reflectWrite(np)(NodePropUpdate(np, idx, x))

  def nodeprop_size[A:Manifest](np: Rep[NodeProperty[A]]):Rep[Int] = reflectPure(NodePropSize(np))

  //////////////
  // mirroring
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case NodePropGetDef(np,i) => nodeprop_get_def(f(np),f(i))
    case NodePropHasDef(np,i) => nodeprop_has_def(f(np),f(i))
    case Reflect(e@NodePropObjectNew(g,s), u, es) => reflectMirrored(Reflect(NodePropObjectNew(f(g),f(s))(e.mNP), mapOver(f,u), f(es)))(mtype(manifest[A]))
    // TODO: change mirroring of changed IR nodes:
    //case Reflect(e@NodePropDefer(np,i,x), u, es) => reflectMirrored(Reflect(NodePropDefer(f(np),f(i),f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodePropGetDef(np,i), u, es) => reflectMirrored(Reflect(NodePropGetDef(f(np),f(i)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodePropHasDef(np,i), u, es) => reflectMirrored(Reflect(NodePropHasDef(f(np),f(i)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodePropClearDef(np,i), u, es) => reflectMirrored(Reflect(NodePropClearDef(f(np),f(i)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodePropAssignAll(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with NodePropAssignAll(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@NodePropSetAll(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with NodePropSetAll(f(x),f(y)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] 
  
}

trait BaseGenNodePropertyOps extends GenericFatCodegen {
  val IR: NodePropertyOpsExp
  import IR._

}

trait ScalaGenNodePropertyOps extends BaseGenNodePropertyOps with ScalaGenFat {
  val IR: NodePropertyOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case np@NodePropObjectNew(g, size) => emitValDef(sym, "new " + remap(np.mNP) +"(" + quote(g) + "," + quote(size) + ")")
      case NodePropApply(np,idx) => emitValDef(sym, quote(np) + ".data(" + quote(idx) + ")")
      case NodePropUpdate(np, idx, x) => emitValDef(sym, quote(np) + ".data(" + quote(idx) + ") = " + quote(x))
      case NodePropGetDef(np, idx) => emitValDef(sym, quote(np) + ".deferred_data(" + quote(idx) + ")")
      case NodePropSetDef(np, idx, x) => emitValDef(sym, quote(np) + ".deferred_data(" + quote(idx) + ") = " + quote(x))
      case NodePropHasDef(np, idx) => emitValDef(sym, quote(np) + ".deferred_data_bitmap(" + quote(idx) + ")")
      case NodePropSetIsDef(np,idx) => emitValDef(sym, quote(np) + ".deferred_data_bitmap(" + quote(idx) + ") = true")
      case NodePropClearDef(np, idx) => emitValDef(sym, quote(np) + ".deferred_data_bitmap(" + quote(idx) + ") = false")
      case NodePropSize(np) => emitValDef(sym, quote(np) + ".size")
      case _ => super.emitNode(sym, rhs)
    }
  }
}