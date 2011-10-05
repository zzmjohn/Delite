package ppl.delite.framework.collections



import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.virtualization.lms.common._
import ppl.delite.framework.collections.datastruct.scala._
import ppl.delite.framework.ops.DeliteOpsExp



trait GenericCollectionsOps extends DSLType with Variables {
  
  /* definition of the CanBuild evidence */
  trait CanBuild[-Coll, -S, +Target] {
    def alloc(source: Rep[Coll]): Rep[Target]
  }
  
}


trait TraversableOps extends GenericCollectionsOps {
  
  /* ctors */
  object Traversable {
  }
  
  /* lifting */
  // TODO: possibly have to isolate this into a supertrait hierarchy (with self types) to ensure proper implicit resolution
  implicit def travrep2traversableops[T: Manifest](t: Rep[Traversable[T]]) = new TraversableClsOps[T, Traversable[T]](t)
  
  class TraversableClsOps[T: Manifest, Coll <: Traversable[T]: Manifest](t: Rep[Coll]) {
    def size: Rep[Int] = traversable_size[T, Coll](t)
    def foreach(block: Rep[T] => Rep[Unit]) = traversable_foreach(t, block)
    def map[S, Target <: DeliteCollection[S]](f: Rep[T] => Rep[S])(implicit cbf: CanBuild[Coll, S, Target], ms: Manifest[S], mt: Manifest[Target]) = traversable_map[T, S, Coll, Target](t, f, cbf)
  }
  
  /* class defs */
  def traversable_size[T: Manifest, Coll <: Traversable[T]: Manifest](t: Rep[Coll]): Rep[Int]
  def traversable_foreach[T: Manifest, Coll <: Traversable[T]: Manifest](t: Rep[Coll], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def traversable_map[T: Manifest, S: Manifest, Coll <: Traversable[T]: Manifest, Target <: DeliteCollection[S]: Manifest](t: Rep[Coll], f: Rep[T] => Rep[S], cbf: CanBuild[Coll, S, Target]): Rep[Target]
  
  /* implicit rules */
  implicit def traversableCanBuild[T: Manifest, S: Manifest, Target <: DeliteCollection[S]: Manifest]: CanBuild[Traversable[T], S, Traversable[S]]
  
}


trait TraversableOpsExp extends TraversableOps with VariablesExp with BaseFatExp with DeliteOpsExp {
self: ArraySeqOpsExp =>
  
  /* nodes */
  case class TraversableSize[T: Manifest, Coll <: Traversable[T]: Manifest](t: Exp[Coll]) extends Def[Int]
  case class TraversableForeach[T: Manifest, Coll <: Traversable[T]: Manifest](in: Exp[Coll], func: Exp[T] => Exp[Unit])
  extends DeliteOpForeach[T] {
    def sync = n => Const(List()) // ? why not: n => List() - where's the implicit to do this??
    val size = copyTransformedOrElse(_.size)(in.size)
  }
  case class TraversableMap[T: Manifest, S: Manifest, Coll <: Traversable[T]: Manifest, Target <: DeliteCollection[S]: Manifest](in: Exp[Coll], func: Exp[T] => Exp[S], cbf: CanBuild[Coll, S, Target])
  extends DeliteOpMap[T, S, Target] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def alloc = cbf.alloc(in)
    
    val mA = manifest[T]
    val mB = manifest[S]
  }
  
  /* class interface */
  def traversable_size[T: Manifest, Coll <: Traversable[T]: Manifest](t: Exp[Coll]) = reflectPure(TraversableSize[T, Coll](t))
  def traversable_foreach[T: Manifest, Coll <: Traversable[T]: Manifest](t: Exp[Coll], block: Exp[T] => Rep[Unit]) = reflectEffect(TraversableForeach[T, Coll](t, block))
  def traversable_map[T: Manifest, S: Manifest, Coll <: Traversable[T]: Manifest, Target <: DeliteCollection[S]: Manifest](t: Exp[Coll], f: Exp[T] => Exp[S], cbf: CanBuild[Coll, S, Target]): Exp[Target] = reflectPure(TraversableMap[T, S, Coll, Target](t, f, cbf))
  
  /* implicit rules */
  implicit def traversableCanBuild[T: Manifest, S: Manifest, Target <: DeliteCollection[S]: Manifest] = new CanBuild[Traversable[T], S, Traversable[S]] {
    def alloc(source: Exp[Traversable[T]]) = ArraySeq.apply[S](travrep2traversableops(source).size)
  }
  
}


trait ScalaGenTraversableOps extends ScalaGenFat {
  val IR: TraversableOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case TraversableSize(t) => emitValDef(sym, quote(t) + ".size")
    case _ => super.emitNode(sym, rhs)
  }
  
}




