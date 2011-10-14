package ppl.delite.framework.collections



import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter



trait MapOps extends TraversableOps {
  
  /* ctors */
  object Map {
  }
  
  /* lifting */
  implicit def maprep2traversableops[K: Manifest, V: Manifest](t: Rep[Map[K, V]]) = new TraversableClsOps[(K, V), Map[K, V]](t)
  
  /* class interface defs */
  
  /* implicit rules */
  implicit def mapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[Map[K, V], (P, Q), Map[P, Q]]
  
}


trait MapOpsExp extends TraversableOpsExp {
self: HashMapOpsExp with HashMapEmitting =>
  
  /* nodes */
  
  /* class interface */
  
  /* implicit rules */
  implicit def mapCanBuild[K: Manifest, V: Manifest, P: Manifest, Q: Manifest]: CanBuild[Map[K, V], (P, Q), Map[P, Q]] = new CanBuild[Map[K, V], (P, Q), Map[P, Q]] {
    def alloc(source: Exp[Map[K, V]]) = HashMap.apply[P, Q]()
    def emptyAlloc(source: Exp[Map[K, V]]) = HashMap[P, Q]()
    def emitterScala(source: Exp[Map[K, V]]) = scalaHashMapEmitter[K, V]
    def noPrealloc = true
  }
  
}


trait ScalaGenMapOps extends ScalaGenTraversableOps {
self: ScalaGenHashMapOps =>
  
  val IR: MapOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case _ => super.emitNode(sym, rhs)
  }
  
}

