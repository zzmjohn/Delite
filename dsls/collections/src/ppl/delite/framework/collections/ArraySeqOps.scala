package ppl.delite.framework.collections



import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import ppl.delite.framework.collections.datastruct.scala._
import ppl.delite.framework.datastruct.scala.DeliteCollection
import java.io.PrintWriter



trait ArraySeqOps extends SeqOps {
  
  /* ctors */
  object ArraySeq {
    def apply[T: Manifest](length: Rep[Int]) = arrayseq_obj_new(length)
  }
  
  /* lifting */
  implicit def arrayseqrep2traversableops[T: Manifest](t: Rep[ArraySeq[T]]): TraversableClsOps[T, ArraySeq[T]] = new TraversableClsOps[T, ArraySeq[T]](t)
  implicit def arrayseqrep2arrayseqops[T: Manifest](arrseq: Rep[ArraySeq[T]]) = new ArraySeqOpsCls(arrseq)
  
  class ArraySeqOpsCls[T: Manifest](arrseq: Rep[ArraySeq[T]]) {
  }
  
  /* object defs */
  def arrayseq_obj_new[T: Manifest](length: Rep[Int]): Rep[ArraySeq[T]]
  
  /* class interface defs */
  
  /* implicit rules */
  implicit def arraySeqCanBuild[T: Manifest, S: Manifest]: CanBuild[ArraySeq[T], S, ArraySeq[S]]
  
}


trait ArraySeqOpsExp extends SeqOpsExp with ArraySeqOps {
  
  // TODO: remove later, but - the implicit conversion below _is_ resolved, unlike the one in the supertrait - why?
  //implicit def arrayseqrep2traversableops2[T: Manifest](t: Rep[ArraySeq[T]]): TraversableClsOps[T, ArraySeq[T]] = new TraversableClsOps[T, ArraySeq[T]](t)
  
  /* node defs */
  case class ArraySeqNew[T](len: Exp[Int])(val mV: Manifest[ArraySeqImpl[T]]) extends Def[ArraySeq[T]]
  
  /* class interface */
  def arrayseq_obj_new[T: Manifest](length: Exp[Int]) = reflectPure(ArraySeqNew[T](length)(manifest[ArraySeqImpl[T]]))
  
  /* can-build rules */
  implicit def arraySeqCanBuild[T: Manifest, S: Manifest] = new CanBuild[ArraySeq[T], S, ArraySeq[S]] {
    def alloc(source: Exp[ArraySeq[T]]) = ArraySeq[S](arrayseqrep2traversableops(source).size)
    // TODO: why doesn't this implicit conversion get resolved automatically? It is available in the current scope without a prefix.
    def emitter(source: Exp[ArraySeq[T]]): Emitter[ArraySeq[S]] = null
  }
  
}


trait ScalaGenArraySeqOps extends ScalaGenSeqOps {
  val IR: ArraySeqOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case v @ ArraySeqNew(length) => emitValDef(sym, "new " + remap(v.mV) + "(" + quote(length) + ")")
    case _ => super.emitNode(sym, rhs)
  }
  
  implicit def canEmitScalaArraySeq[T] = new CanEmit[ArraySeq[T], ScalaTarget] {
    def emitBufferDefs(basename: String)(implicit stream: PrintWriter) {
    }
    def emitInitSubActivation(basename: String, activname: String)(implicit stream: PrintWriter) {
    }
    def emitAddToBuffer(basename: String, activname: String)(implicit stream: PrintWriter) {
    }
    def emitPostCombine(basename: String, activname: String, lhsname: String)(implicit stream: PrintWriter) {
    }
    def emitPostProcInit(basename: String, activname: String)(implicit stream: PrintWriter) {
    }
    def emitPostProcess(basename: String, activname: String)(implicit stream: PrintWriter) {
    }
  }
}






