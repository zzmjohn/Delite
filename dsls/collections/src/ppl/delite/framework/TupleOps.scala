package ppl.delite.framework.collections



import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.ops.DeliteCollectionOps
import scala.reflect.SourceContext



trait TupleOpsExpOpt extends TupleOpsExp {
self: LongOpsExp with PrimitiveOpsExp =>
  
  override implicit def make_tuple2[A:Manifest,B:Manifest](t: (Exp[A],Exp[B]))(implicit ctx: SourceContext): Exp[(A,B)] = ETuple2(t._1, t._2)  
  
  case class ETuple2IntInt(encoding: Exp[Long]) extends Def[(Int, Int)]
  
  override def tuple2_get1[A: Manifest](t: Exp[(A, _)])(implicit ctx: SourceContext) = t match {
    //case ETuple2IntInt(enc) => LongURShift(LongAnd(enc, 0xffffffff00000000L), 32)
    case _ => super.tuple2_get1[A](t)
  }
  
  override def tuple2_get2[B: Manifest](t: Exp[(_, B)])(implicit ctx: SourceContext) = t match {
    //case ETuple2IntInt(enc) => LongAnd(enc, 0xffffffffL)
    case _ => super.tuple2_get2[B](t)
  }
  
}


trait ScalaGenTupleOpsOpt extends ScalaGenTupleOps {
  
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case e@ETuple2(a, b) if e.m1 == manifest[Int] && e.m2 == manifest[Int] =>
      emitValDef(sym, "("+ quote(a) + ".toLong << 32) + " + quote(b)) 
    case a@Tuple2Access1(t) if a.m == manifest[Int] =>
      emitValDef(sym, "((" + quote(t) + " & 0xffffffff00000000L) >>> 32).toInt")
    case a@Tuple2Access2(t) if a.m == manifest[Int] =>
      emitValDef(sym, "(" + quote(t) + " & 0xffffffffL).toInt")
    case _ =>
      super.emitNode(sym, rhs)
  }
  
}
