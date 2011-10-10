package ppl.delite.framework.collections



import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter
import ppl.delite.framework.datastruct.scala._



trait ArrayBufferOps extends SeqOps {
  
  /* ctors */
  // TODO - array buffer
  object Buffer {
    def apply[T: Manifest](length: Rep[Int]) = arraybuffer_obj_new(length)
  }
  
  /* lifting */
  implicit def arraybufferrep2traversableops[T: Manifest](t: Rep[ArrayBuffer[T]]): TraversableClsOps[T, ArrayBuffer[T]] = new TraversableClsOps[T, ArrayBuffer[T]](t)
  implicit def arraybufferrep2arraybufferops[T: Manifest](arrseq: Rep[ArrayBuffer[T]]) = new ArrayBufferOpsCls(arrseq)
  
  class ArrayBufferOpsCls[T: Manifest](arrseq: Rep[ArrayBuffer[T]]) {
  }
  
  /* object defs */
  def arraybuffer_obj_new[T: Manifest](length: Rep[Int]): Rep[ArrayBuffer[T]]
  
  /* class interface defs */
  
  /* implicit rules */
  implicit def arrayBufferCanBuild[T: Manifest, S: Manifest]: CanBuild[ArrayBuffer[T], S, ArrayBuffer[S]]
  
}


trait ArrayBufferOpsExp extends SeqOpsExp with ArrayBufferOps {
self: ArrayBufferEmitting =>
  
  // TODO: remove later, but - the implicit conversion below _is_ resolved, unlike the one in the supertrait - why?
  //implicit def arraybufferrep2traversableops2[T: Manifest](t: Rep[ArrayBuffer[T]]): TraversableClsOps[T, ArrayBuffer[T]] = new TraversableClsOps[T, ArrayBuffer[T]](t)
  
  /* node defs */
  case class BufferNew[T](len: Exp[Int])(val mV: Manifest[ArrayBufferImpl[T]]) extends Def[ArrayBuffer[T]]
  
  /* class interface */
  def arraybuffer_obj_new[T: Manifest](length: Exp[Int]) = reflectPure(BufferNew[T](length)(manifest[ArrayBufferImpl[T]]))
  
  /* can-build rules */
  implicit def arrayBufferCanBuild[T: Manifest, S: Manifest] = new CanBuild[ArrayBuffer[T], S, ArrayBuffer[S]] {
    // TODO: why doesn't this implicit conversion get resolved automatically? It is available in the current scope without a prefix.
    def alloc(source: Exp[ArrayBuffer[T]]) = Buffer[S](arraybufferrep2traversableops(source).size)
    def emptyAlloc(source: Exp[ArrayBuffer[T]]) = Buffer[S](Const(0))
    def emitterScala(source: Exp[ArrayBuffer[T]]): ScalaEmitter = scalaArrayBufferEmitter[T]
  }
  
}


trait ScalaGenArrayBufferOps extends ScalaGenSeqOps {
  val IR: ArrayBufferOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v @ BufferNew(length) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(length) + ")")
    case _ => super.emitNode(sym, rhs)
  }
  
}



