package ppl.delite.framework.collections



import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import ppl.delite.framework.collections.datastruct.scala._



trait ArraySeqOps extends SeqOps {
  
  object ArraySeq {
    def apply[T: Manifest](length: Int) = arrayseq_obj_new(length)
  }
  
  implicit def rep2arrayseqops[T: Manifest](arrseq: Rep[ArraySeq[T]]) = new ArraySeqOpsCls(arrseq)
  
  class ArraySeqOpsCls[T: Manifest](arrseq: Rep[ArraySeq[T]]) {
    
  }
  
  // object defs
  def arrayseq_obj_new[T: Manifest](length: Int): Rep[ArraySeq[T]]
  
  // class defs
  
}


trait ArraySeqOpsExp extends SeqOpsExp {
  
  
  
}


