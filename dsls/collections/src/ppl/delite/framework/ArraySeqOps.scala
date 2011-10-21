package ppl.delite.framework.collections



import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter
import ppl.delite.framework.datastruct.scala._



trait ArraySeqOps extends SeqOps {
  
  /* ctors */
  // TODO - array buffer
  object ArraySeq {
    def apply[T: Manifest](length: Rep[Int]) = arraybuffer_obj_new(length)
  }
  
  /* lifting */
  implicit def arraybufferrep2traversableops[T: Manifest](t: Rep[ArraySeq[T]]): TraversableClsOps[T, ArraySeq[T]] = new TraversableClsOps[T, ArraySeq[T]](t)
  implicit def arraybufferrep2arraybufferops[T: Manifest](arrseq: Rep[ArraySeq[T]]) = new ArraySeqOpsCls(arrseq)
  
  class ArraySeqOpsCls[T: Manifest](arrseq: Rep[ArraySeq[T]]) {
  }
  
  /* object defs */
  def arraybuffer_obj_new[T: Manifest](length: Rep[Int]): Rep[ArraySeq[T]]
  
  /* class interface defs */
  
  /* implicit rules */
  implicit def arrayBufferCanBuild[T: Manifest, S: Manifest]: CanBuild[ArraySeq[T], S, ArraySeq[S]]
  
}


trait ArraySeqOpsExp extends SeqOpsExp with ArraySeqOps {
self: ArraySeqEmitting =>
  
  // TODO: remove later, but - the implicit conversion below _is_ resolved, unlike the one in the supertrait - why?
  //implicit def arraybufferrep2traversableops2[T: Manifest](t: Rep[ArraySeq[T]]): TraversableClsOps[T, ArraySeq[T]] = new TraversableClsOps[T, ArraySeq[T]](t)
  
  /* node defs */
  case class BufferNew[T](len: Exp[Int])(val mV: Manifest[ArraySeqImpl[T]]) extends Def[ArraySeq[T]]
  
  /* class interface */
  def arraybuffer_obj_new[T: Manifest](length: Exp[Int]) = reflectPure(BufferNew[T](length)(manifest[ArraySeqImpl[T]]))
  
  /* can-build rules */
  implicit def arrayBufferCanBuild[T: Manifest, S: Manifest] = new CanBuild[ArraySeq[T], S, ArraySeq[S]] {
    // TODO: why doesn't this implicit conversion get resolved automatically? It is available in the current scope without a prefix.
    def alloc(source: Exp[ArraySeq[T]]) = ArraySeq[S](arraybufferrep2traversableops(source).size)
    def emptyAlloc(source: Exp[ArraySeq[T]]) = ArraySeq[S](Const(0))
    def emitterFactory(source: Exp[ArraySeq[T]]) = arraySeqEmitterFactory[T]
    def noPrealloc = false
  }
  
}


trait ScalaGenArraySeqOps extends ScalaGenSeqOps {
  val IR: ArraySeqOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v @ BufferNew(length) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(length) + ")")
    case _ => super.emitNode(sym, rhs)
  }
  
}



