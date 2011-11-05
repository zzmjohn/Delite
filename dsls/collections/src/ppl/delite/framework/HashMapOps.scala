package ppl.delite.framework.collections



import ppl.delite.framework.DeliteApplication
import scala.virtualization.lms.common._
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter
import ppl.dsl.optila.{OptiLA, OptiLAExp}



trait HashMap1Ops extends MapOps {
self: OptiLA with ArraySeqOps with ArraySeqEmitting =>
  
  /* ctors */
  object HashMap1 {
    def apply[K: Manifest, V: Manifest]() = hashmap1_obj_new[K, V]()
    def range(n: Rep[Int]) = hashmap1_obj_new_range(n)
  }
  
  /* lifting */
  implicit def hashmaprep2traversableops[K: Manifest, V: Manifest](t: Rep[HashMap[K, V]]) = new TraversableClsOps[(K, V), HashMap[K, V]](t)
  
  /* object defs */
  def hashmap1_obj_new[K: Manifest, V: Manifest](): Rep[HashMap[K, V]]
  def hashmap1_obj_new_range(n: Rep[Int]): Rep[HashMap[Int, Int]]
  
  /* class interface defs */
  
  /* implicit rules */
  implicit def hashMapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[HashMap[K, V], (P, Q), HashMap[P, Q]]
  
}


trait HashMap1OpsExp extends MapOpsExp with HashMap1Ops {
self: OptiLAExp with HashMap1OpsExp with HashMapEmitting with ArraySeqOpsExp with ArraySeqEmitting with HashMultiMapEmitting =>
  
  /* nodes */
  case class HashMap1New[K, V]()(val mV: Manifest[HashMapImpl[K, V]]) extends Def[HashMap[K, V]]
  case class HashMap1NewRange(n: Exp[Int])(val mV: Manifest[HashMapImpl[Int, Int]]) extends Def[HashMap[Int, Int]]
  
  /* lifting */
  
  /* class interface */
  
  /* implicit rules */
  implicit def hashMapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[HashMap[K, V], (P, Q), HashMap[P, Q]] = new CanBuild[HashMap[K, V], (P, Q), HashMap[P, Q]] {
    def alloc(source: Exp[HashMap[K, V]]) = HashMap1[P, Q]()
    def emptyAlloc(source: Exp[HashMap[K, V]]) = HashMap1[P, Q]().asInstanceOf[Def[HashMap[P, Q]]]
    def emitterFactory(source: Exp[HashMap[K, V]]) = hashMapEmitterFactory[K, V]
    def noPrealloc = true
  }
  
  def hashmap1_obj_new[K: Manifest, V: Manifest](): Exp[HashMap[K, V]] = reflectMutable(HashMap1New[K, V]()(manifest[HashMapImpl[K, V]]))
  def hashmap1_obj_new_range(n: Exp[Int]) = reflectMutable(HashMap1NewRange(n)(manifest[HashMapImpl[Int, Int]]))
  
}


trait ScalaGenHashMap1Ops extends ScalaGenMapOps {
//self: ScalaGenHashMapOps =>
  
  val IR: HashMap1OpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v @ HashMap1New() => emitValDef(sym, "new " + remap(v.mV) + "()")
    case v @ HashMap1NewRange(n) =>
      emitValDef(sym, "HashMapImpl.range(%s)".format(quote(n)))
    case _ => super.emitNode(sym, rhs)
  }
  
}





