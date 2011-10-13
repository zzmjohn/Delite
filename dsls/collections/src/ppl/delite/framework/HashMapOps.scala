package ppl.delite.framework.collections



import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter



trait HashMapOps extends MapOps {
  
  /* ctors */
  object HashMap {
    def apply[K: Manifest, V: Manifest]() = hashmap_obj_new[K, V]()
  }
  
  /* lifting */
  implicit def hashmaprep2traversableops[K: Manifest, V: Manifest](t: Rep[HashMap[K, V]]) = new TraversableClsOps[(K, V), HashMap[K, V]](t)
  
  /* object defs */
  def hashmap_obj_new[K: Manifest, V: Manifest](): Rep[HashMap[K, V]]
  
  /* class interface defs */
  
  /* implicit rules */
  implicit def hashMapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[HashMap[K, V], (P, Q), HashMap[P, Q]]
  
}


trait HashMapOpsExp extends MapOpsExp with HashMapOps {
self: HashMapOpsExp with HashMapEmitting =>
  
  /* nodes */
  case class HashMapNew[K, V]()(val mV: Manifest[HashMapImpl[K, V]]) extends Def[HashMap[K, V]]
  
  /* class interface */
  
  /* implicit rules */
  implicit def hashMapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[HashMap[K, V], (P, Q), HashMap[P, Q]] = new CanBuild[HashMap[K, V], (P, Q), HashMap[P, Q]] {
    def alloc(source: Exp[HashMap[K, V]]) = HashMap.apply[P, Q]()
    def emptyAlloc(source: Exp[HashMap[K, V]]) = HashMap[P, Q]()
    def emitterScala(source: Exp[HashMap[K, V]]) = scalaHashMapEmitter[K, V]
  }
  
  def hashmap_obj_new[K: Manifest, V: Manifest](): Exp[HashMap[K, V]] = HashMapNew[K, V]()(manifest[HashMapImpl[K, V]])
  
}


trait ScalaGenHashMapOps extends ScalaGenMapOps {
//self: ScalaGenHashMapOps =>
  
  val IR: HashMapOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v @ HashMapNew() => emitValDef(sym, "new " + remap(v.mV) + "()")
    case _ => super.emitNode(sym, rhs)
  }
  
}

