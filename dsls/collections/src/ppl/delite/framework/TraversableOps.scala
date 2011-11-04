package ppl.delite.framework.collections



import java.io.PrintWriter
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.virtualization.lms.common._
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.ops.DeliteCollectionOps



trait LowPriorityCollectionImplicits extends GenericCollectionOps {
  
  // TODO see what to can be done about this
  //implicit def deliteCollectionCanBuild[T: Manifest, S: Manifest]: CanBuild[DeliteCollection[T], S, DeliteCollection[S]] = sys.error("unimplemented")
  
}


trait TraversableOps extends GenericCollectionOps with DeliteCollectionOps with LowPriorityCollectionImplicits {
  
  /* ctors */
  object Traversable {
  }
  
  /* lifting */
  // TODO: possibly have to isolate this into a supertrait hierarchy (with self types) to ensure proper implicit resolution
  implicit def travrep2traversableops[T: Manifest](t: Rep[Traversable[T]]) = new TraversableClsOps[T, Traversable[T]](t)
  implicit def liftUnit(u: Unit): Rep[Unit]
  
  class TraversableClsOps[T: Manifest, Coll <: DeliteCollection[T]: Manifest](t: Rep[Coll]) {
    def size: Rep[Int] = traversable_size[T, Coll](t)
    def map[S, Target <: DeliteCollection[S]](f: Rep[T] => Rep[S])(implicit cbf: CanBuild[Coll, S, Target], ms: Manifest[S], mt: Manifest[Target]) = traversable_map[T, S, Coll, Target](t, f, cbf)
    def filter[Target <: DeliteCollection[T]](p: Rep[T] => Rep[Boolean])(implicit cbf: CanBuild[Coll, T, Target], ms: Manifest[Target]) = traversable_filter[T, Coll, Target](t, p, cbf)
    def groupBy[K: Manifest](f: Rep[T] => Rep[K]) = traversable_groupby[T, Coll, K](t, f)
    def flatMap[S, Target <: DeliteCollection[S]](f: Rep[T] => Rep[DeliteCollection[S]])(implicit cbf: CanBuild[Coll, S, Target], ms: Manifest[S], mt: Manifest[Target]) = traversable_flatmap[T, S, Coll, Target](t, f, cbf)
    
    def foreach(block: Rep[T] => Rep[Unit]) = traversable_foreach(t, block)
    def sumBy[S: Manifest: Numeric](func: Rep[T] => Rep[S]) = traversable_sumby(t, func)
  }
  
  /* class interface defs */
  def traversable_size[T: Manifest, Coll <: DeliteCollection[T]: Manifest](t: Rep[Coll]): Rep[Int]
  def traversable_map[T: Manifest, S: Manifest, Coll <: DeliteCollection[T]: Manifest, Target <: DeliteCollection[S]: Manifest](t: Rep[Coll], f: Rep[T] => Rep[S], cbf: CanBuild[Coll, S, Target]): Rep[Target]
  def traversable_filter[T: Manifest, Coll <: DeliteCollection[T]: Manifest, Target <: DeliteCollection[T]: Manifest](t: Rep[Coll], p: Rep[T] => Rep[Boolean], cbf: CanBuild[Coll, T, Target]): Rep[Target]
  def traversable_groupby[T: Manifest, Coll <: DeliteCollection[T]: Manifest, K: Manifest](in: Rep[Coll], f: Rep[T] => Rep[K]): Rep[HashMap[K, Bucket[T]]]
  def traversable_flatmap[T: Manifest, S: Manifest, Coll <: DeliteCollection[T]: Manifest, Target <: DeliteCollection[S]: Manifest](t: Rep[Coll], f: Rep[T] => Rep[DeliteCollection[S]], cbf: CanBuild[Coll, S, Target]): Rep[Target]
  def traversable_foreach[T: Manifest, Coll <: DeliteCollection[T]: Manifest](t: Rep[Coll], block: Rep[T] => Rep[Unit]): Rep[Unit]
  def traversable_sumby[T: Manifest, Coll <: DeliteCollection[T]: Manifest, S: Manifest: Numeric](t: Rep[Coll], func: Rep[T] => Rep[S]): Rep[S]
  
  /* implicit rules */
  implicit def traversableCanBuild[T: Manifest, S: Manifest]: CanBuild[Traversable[T], S, Traversable[S]]
  
}


trait TraversableOpsExp extends TraversableOps with VariablesExp with TupleOpsExp with BaseFatExp with DeliteOpsExp {
self: HashMapOpsExp with HashMultiMapEmitting with ArraySeqOpsExp with ArraySeqEmitting =>
  
  /* lifting */
  implicit def liftUnit(u: Unit): Exp[Unit] = Const(())
  
  /* nodes */
  case class TraversableSize[T: Manifest, Coll <: DeliteCollection[T]: Manifest](t: Exp[Coll]) extends Def[Int]
  case class TraversableForeach[T: Manifest, Coll <: DeliteCollection[T]: Manifest](in: Exp[Coll], func: Exp[T] => Exp[Unit])
  extends DeliteOpForeach[T] {
    def sync = n => Const(List()) // ? why not: n => List() - where's the implicit to do this??
    val size = copyTransformedOrElse(_.size)(in.size)
  }
  case class TraversableMap[T: Manifest, S: Manifest, Coll <: DeliteCollection[T]: Manifest, Target <: DeliteCollection[S]: Manifest]
  (in: Exp[Coll], func: Exp[T] => Exp[S], cbf: CanBuild[Coll, S, Target])
  extends DeliteOpMap[T, S, Target] {
    val size = copyTransformedOrElse(_.size)(in.size)
    def alloc = cbf.alloc(in)
    override def emitterFactory = Some(cbf.emitterFactory(in))
    
    val mA = manifest[T]
    val mB = manifest[S]
  }
  case class TraversableFilter[T: Manifest, Coll <: DeliteCollection[T]: Manifest, Target <: DeliteCollection[T]: Manifest]
  (in: Exp[Coll], pred: Exp[T] => Exp[Boolean], cbf: CanBuild[Coll, T, Target])
  extends DeliteOpFilter[T, T, Target] {
    def alloc = cbf.alloc(in)
    def func = e => e
    def cond = pred
    val size = in.size
    override def emitterFactory = Some(cbf.emitterFactory(in))
    
    def m = manifest[T]
  }
  case class TraversableGroupBy[T: Manifest, K: Manifest, Coll <: DeliteCollection[T]: Manifest]
  (in: Exp[Coll], f: Exp[T] => Exp[K])
  extends DeliteOpGroupBy[T, K, T, Coll, Bucket[T], HashMap[K, Bucket[T]]] {
    val size = in.size
    def funcKey: Exp[T] => Exp[K] = x => f(x)
    def funcVal: Exp[T] => Exp[T] = x => x
    def alloc: Exp[HashMap[K, Bucket[T]]] = HashMapNew[K, Bucket[T]]()(manifest[HashMapImpl[K, Bucket[T]]])
    def convertToCV: (Exp[K], Exp[Bucket[T]]) => Exp[Bucket[T]] = (k, x) => x
    def emitterFactory: Option[EmitterFactory] = Some(hashMultiMapEmitterFactory)
  }
  case class TraversableFlatMap[T: Manifest, S: Manifest, Coll <: DeliteCollection[T]: Manifest, Target <: DeliteCollection[S]: Manifest]
  (in: Exp[Coll], func: Exp[T] => Exp[DeliteCollection[S]], cbf: CanBuild[Coll, S, Target])
  extends DeliteOpFlatMap[T, S, Target] {
    val size = in.size
    def alloc = cbf.alloc(in)
    override def emitterFactory = Some(cbf.emitterFactory(in))
  }
  case class TraversableSumBy[T: Manifest, S: Manifest: Numeric, Coll <: DeliteCollection[T]: Manifest]
  (in: Exp[Coll], map: Exp[T] => Exp[S])
  extends DeliteOpMapReduce[T, S] {
    val size = in.size
    val reduce = (a: Exp[S], b: Exp[S]) => ???
    val zero = ???
  }
  
  /* class interface */
  def traversable_size[T: Manifest, Coll <: DeliteCollection[T]: Manifest](t: Exp[Coll]) = reflectPure(TraversableSize[T, Coll](t))
  def traversable_map[T: Manifest, S: Manifest, Coll <: DeliteCollection[T]: Manifest, Target <: DeliteCollection[S]: Manifest](t: Exp[Coll], f: Exp[T] => Exp[S], cbf: CanBuild[Coll, S, Target]): Exp[Target] = reflectEffect(TraversableMap[T, S, Coll, Target](t, f, cbf))
  def traversable_filter[T: Manifest, Coll <: DeliteCollection[T]: Manifest, Target <: DeliteCollection[T]: Manifest](t: Exp[Coll], p: Exp[T] => Exp[Boolean], cbf: CanBuild[Coll, T, Target]): Exp[Target] = reflectEffect(TraversableFilter[T, Coll, Target](t, p, cbf))
  def traversable_groupby[T: Manifest, Coll <: DeliteCollection[T]: Manifest, K: Manifest](in: Exp[Coll], f: Exp[T] => Exp[K]): Exp[HashMap[K, Bucket[T]]] = reflectEffect(TraversableGroupBy[T, K, Coll](in, f))
  def traversable_flatmap[T: Manifest, S: Manifest, Coll <: DeliteCollection[T]: Manifest, Target <: DeliteCollection[S]: Manifest](t: Exp[Coll], f: Exp[T] => Exp[DeliteCollection[S]], cbf: CanBuild[Coll, S, Target]): Exp[Target] = reflectEffect(TraversableFlatMap[T, S, Coll, Target](t, f, cbf))
  def traversable_foreach[T: Manifest, Coll <: DeliteCollection[T]: Manifest](t: Exp[Coll], block: Exp[T] => Rep[Unit]) = reflectEffect(TraversableForeach[T, Coll](t, block))
  def traversable_sumby[T: Manifest, Coll <: DeliteCollection[T]: Manifest, S: Manifest: Numeric](t: Exp[Coll], func: Exp[T] => Exp[S]): Exp[S] = reflectEffect(TraversableSumBy[T, S, Coll](t, func))
  
  /* implicit rules */
  implicit def traversableCanBuild[T: Manifest, S: Manifest] = new CanBuild[Traversable[T], S, Traversable[S]] {
    def alloc(source: Exp[Traversable[T]]) = ArraySeq.apply[S](travrep2traversableops(source).size)
    def emptyAlloc(source: Exp[Traversable[T]]) = ArraySeq[S](Const(0))
    def emitterFactory(source: Exp[Traversable[T]]) = arraySeqEmitterFactory[T]
    def noPrealloc = false
  }
  
}


trait ScalaGenTraversableOps extends ScalaGenFat with ScalaGenTupleOps with GenericCollectionGen {
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




