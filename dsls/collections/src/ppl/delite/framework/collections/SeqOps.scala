package ppl.delite.framework.collections



import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import ppl.delite.framework.collections.datastruct.scala._
import java.io.PrintWriter



trait SeqOps extends TraversableOps {
  
  implicit def seqrep2traversableops[T: Manifest](t: Rep[Seq[T]]) = new TraversableClsOps[T, Seq[T]](t)
  
}


trait SeqOpsExp extends TraversableOpsExp {
  
  
  
}


trait ScalaGenSeqOps extends ScalaGenTraversableOps {
  val IR: SeqOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case _ => super.emitNode(sym, rhs)
  }
  
}

