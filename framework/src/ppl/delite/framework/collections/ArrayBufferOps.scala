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
  
  // TODO: remove later, but - the implicit conversion below _is_ resolved, unlike the one in the supertrait - why?
  //implicit def arraybufferrep2traversableops2[T: Manifest](t: Rep[ArrayBuffer[T]]): TraversableClsOps[T, ArrayBuffer[T]] = new TraversableClsOps[T, ArrayBuffer[T]](t)
  
  /* node defs */
  case class BufferNew[T](len: Exp[Int])(val mV: Manifest[ArrayBufferImpl[T]]) extends Def[ArrayBuffer[T]]
  
  /* class interface */
  def arraybuffer_obj_new[T: Manifest](length: Exp[Int]) = reflectPure(BufferNew[T](length)(manifest[ArrayBufferImpl[T]]))
  
}


trait ScalaGenArrayBufferOps extends ScalaGenSeqOps {
  val IR: ArrayBufferOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v @ BufferNew(length) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(length) + ")")
    case _ => super.emitNode(sym, rhs)
  }
  
  /* can-build rules */
  implicit def arrayBufferCanBuild[T: Manifest, S: Manifest] = new CanBuild[ArrayBuffer[T], S, ArrayBuffer[S]] {
    // TODO: why doesn't this implicit conversion get resolved automatically? It is available in the current scope without a prefix.
    def alloc(source: Exp[ArrayBuffer[T]]) = Buffer[S](arraybufferrep2traversableops(source).size)
    def emptyAlloc(source: Exp[ArrayBuffer[T]]) = Buffer[S](Const(0))
    def emitter(source: Exp[ArrayBuffer[T]]): Emitter = scalaArrayBufferEmitter[T]
  }
  
  def scalaArrayBufferEmitter[T] = new Emitter {
    def emitBufferDefs(basename: String, elemtype: String)(implicit stream: PrintWriter) {
      stream.println("var " + basename + "_buf: Array[" + elemtype + "] = _")
      stream.println("var " + basename + "_size = 0")
      stream.println("var " + basename + "_offset = 0")
      stream.println("def " + basename + "_buf_init: Unit = {"/*}*/)
      stream.println(basename + "_buf = new Array(128)")
      stream.println(/*{*/"}")
      stream.println("def " + basename + "_buf_append(x: " + elemtype + "): Unit = {"/*}*/)
      stream.println("if (" + basename + "_size >= " + basename + "_buf.length) {"/*}*/)
      stream.println("val old = " + basename + "_buf")
      stream.println(basename + "_buf = new Array(2*old.length)")
      stream.println("System.arraycopy(old, 0, " + basename + "_buf, 0, old.length)")
      stream.println(/*{*/"}")
      stream.println(basename + "_buf(" + basename + "_size) = x")
      stream.println(basename + "_size += 1")
      stream.println(/*{*/"}")
    }
    def emitInitSubActivation(basename: String, activname: String)(implicit stream: PrintWriter) {
      stream.println(activname + "." + basename + "_buf_init")
    }
    def emitAddToBuffer(prefixSym: String, basename: String, elemname: String)(implicit stream: PrintWriter) {
      stream.println(prefixSym + basename + "_buf_append(" + elemname + ")")
    }
    def emitPostCombine(basename: String, activname: String, lhsname: String)(implicit stream: PrintWriter) {
      stream.println(activname + "." + basename + "_offset = " + lhsname + "." + basename + "_offset + " + lhsname + "." + basename + "_size")
    }
    def emitPostProcInit(basename: String, activname: String)(implicit stream: PrintWriter) {
      stream.println("if (" + activname + "." + basename + "_offset > 0) {"/*}*/) // set data array for result object
      stream.println("val len = " + activname + "." + basename + "_offset + " + activname + "." + basename + "_size")
      stream.println("" + activname + "." + basename + ".unsafeSetData(new Array(len), len)")
      stream.println(/*{*/"} else {"/*}*/)
      stream.println("" + activname + "." + basename + ".unsafeSetData(" + activname + "." +basename + "_buf, " + activname + "." + basename + "_size)")
      stream.println(/*{*/"}")
    }
    def emitPostProcess(basename: String, activname: String)(implicit stream: PrintWriter) {
      stream.println("if (" + activname + "." + basename + ".unsafeData ne " + activname + "." + basename + "_buf)")
      stream.println("System.arraycopy(" + activname + "." + basename + "_buf, 0, " + activname + "." + basename + ".unsafeData, " + activname + "." + basename + "_offset, " + activname + "." + basename + "_size)")
      stream.println("" + activname + "." + basename + "_buf = null")
    }
  }
  
}






