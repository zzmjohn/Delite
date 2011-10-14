package ppl.delite.framework.collections



import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.virtualization.lms.common._
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.ops.DeliteOpsExp



trait TraversableOps extends GenericCollectionOps {
  
  /* ctors */
  object Traversable {
  }
  
  /* lifting */
  // TODO: possibly have to isolate this into a supertrait hierarchy (with self types) to ensure proper implicit resolution
  implicit def travrep2traversableops[T: Manifest](t: Rep[Traversable[T]]) = new TraversableClsOps[T, Traversable[T]](t)
  implicit def liftUnit(u: Unit): Rep[Unit]
  
  class TraversableClsOps[T: Manifest, Coll <: Traversable[T]: Manifest](t: Rep[Coll]) {
    def size: Rep[Int] = traversable_size[T, Coll](t)
    def foreach(block: Rep[T] => Rep[Unit]) = traversable_foreach(t, block)
    def map[S, Target <: DeliteCollection[S]](f: Rep[T] => Rep[S])(implicit cbf: CanBuild[Coll, S, Target], ms: Manifest[S], mt: Manifest[Target]) = traversable_map[T, S, Coll, Target](t, f, cbf)
    def filter[Target <: DeliteCollection[T]](p: Rep[T] => Rep[Boolean])(implicit cbf: CanBuild[Coll, T, Target], ms: Manifest[Target]) = traversable_filter[T, Coll, Target](t, p, cbf)
  }
  
  /* class interface defs */
  def traversable_size[T: Manifest, Coll <: Traversable[T]: Manifest](t: Rep[Coll]): Rep[Int]
  def traversable_foreach[T: Manifest, Coll <: Traversable[T]: Manifest](t: Rep[Coll], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def traversable_map[T: Manifest, S: Manifest, Coll <: Traversable[T]: Manifest, Target <: DeliteCollection[S]: Manifest](t: Rep[Coll], f: Rep[T] => Rep[S], cbf: CanBuild[Coll, S, Target]): Rep[Target]
  def traversable_filter[T: Manifest, Coll <: Traversable[T]: Manifest, Target <: DeliteCollection[T]: Manifest](t: Rep[Coll], p: Rep[T] => Rep[Boolean], cbf: CanBuild[Coll, T, Target]): Rep[Target]
  
  /* implicit rules */
  implicit def traversableCanBuild[T: Manifest, S: Manifest]: CanBuild[Traversable[T], S, Traversable[S]]
  
}


trait TraversableOpsExp extends TraversableOps with VariablesExp with BaseFatExp with DeliteOpsExp {
//self: ArraySeqOpsExp with ArraySeqEmitting =>
  
  /* lifting */
  implicit def liftUnit(u: Unit): Exp[Unit] = Const(())
  
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
    override def emitterScala = if (cbf.noPrealloc) Some(cbf.emitterScala(in)) else None
    
    val mA = manifest[T]
    val mB = manifest[S]
  }
  case class TraversableFilter[T: Manifest, Coll <: Traversable[T]: Manifest, Target <: DeliteCollection[T]: Manifest](in: Exp[Coll], pred: Exp[T] => Exp[Boolean], cbf: CanBuild[Coll, T, Target])
  extends DeliteOpFilter[T, T, Target] {
    def alloc = cbf.alloc(in)
    def func = e => e
    def cond = pred
    val size = in.size
    override def emitterScala = Some(cbf.emitterScala(in))
    
    def m = manifest[T]
  }
  
  /* class interface */
  def traversable_size[T: Manifest, Coll <: Traversable[T]: Manifest](t: Exp[Coll]) = reflectPure(TraversableSize[T, Coll](t))
  def traversable_foreach[T: Manifest, Coll <: Traversable[T]: Manifest](t: Exp[Coll], block: Exp[T] => Rep[Unit]) = reflectEffect(TraversableForeach[T, Coll](t, block))
  def traversable_map[T: Manifest, S: Manifest, Coll <: Traversable[T]: Manifest, Target <: DeliteCollection[S]: Manifest](t: Exp[Coll], f: Exp[T] => Exp[S], cbf: CanBuild[Coll, S, Target]): Exp[Target] = reflectEffect(TraversableMap[T, S, Coll, Target](t, f, cbf))
  def traversable_filter[T: Manifest, Coll <: Traversable[T]: Manifest, Target <: DeliteCollection[T]: Manifest](t: Exp[Coll], p: Exp[T] => Exp[Boolean], cbf: CanBuild[Coll, T, Target]): Exp[Target] = reflectEffect(TraversableFilter[T, Coll, Target](t, p, cbf))
  
  /* implicit rules */
  implicit def traversableCanBuild[T: Manifest, S: Manifest] = new CanBuild[Traversable[T], S, Traversable[S]] {
    def alloc(source: Exp[Traversable[T]]) = null //ArraySeq.apply[S](travrep2traversableops(source).size)
    def emptyAlloc(source: Exp[Traversable[T]]) = null //ArraySeq[S](Const(0))
    def emitterScala(source: Exp[Traversable[T]]) = null //scalaArraySeqEmitter[T]
    def noPrealloc = false
  }
  
}


trait ScalaGenTraversableOps extends ScalaGenFat with GenericCollectionGen {
//self: ScalaGenArraySeqOps =>
  
  val IR: TraversableOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    rhs match {
      // these are the ops that call through to the underlying real data structure
      case TraversableSize(t) => emitValDef(sym, quote(t) + ".size")
      case _ =>
        super.emitNode(sym, rhs)
    }
  }
  
}




