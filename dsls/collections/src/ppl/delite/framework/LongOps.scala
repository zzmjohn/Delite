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



trait LowPriorityLongImplicits {
this: Variables with ImplicitOps =>

  implicit def longToRepLong(i: Long): Rep[Long] = unit(i)

  implicit def repIntToRepLong(x: Rep[Int]): Rep[Long] = implicit_convert[Int, Long](x)
}


trait LongOps extends Variables with OverloadHack with LowPriorityLongImplicits {
this: ImplicitOps =>
  
  /**
   * Long
   */
  object Long {
    def parseLong(s: Rep[String])(implicit ctx: SourceContext) = obj_long_parse_long(s)
  }
  
  //def infix_+(lhs: Rep[Long], rhs: Rep[Long])(implicit ctx: SourceContext) = long_plus(lhs, rhs)
  def infix_<<(lhs: Rep[Long], rhs: Rep[Int])(implicit ctx: SourceContext) = long_lshift(lhs, rhs)
  //def infix_&(lhs: Rep[Long], rhs: Rep[Long])(implicit ctx: SourceContext) = long_and(lhs, rhs)
  def infix_>>>(lhs: Rep[Long], rhs: Rep[Int])(implicit ctx: SourceContext) = long_urshift(lhs, rhs)
  
  def obj_long_parse_long(s: Rep[String])(implicit ctx: SourceContext): Rep[Long]
  def long_plus(lhs: Rep[Long], rhs: Rep[Long])(implicit ctx: SourceContext): Rep[Long]
  def long_lshift(lhs: Rep[Long], rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Long]
  def long_and(lhs: Rep[Long], rhs: Rep[Long])(implicit ctx: SourceContext): Rep[Long]
  def long_urshift(lhs: Rep[Long], rhs: Rep[Int])(implicit ctx: SourceContext): Rep[Long]
}


trait LongOpsExp extends LongOps with BaseExp {
this: ImplicitOps =>
  
  /**
   * Long
   */
  case class ObjLongParseLong(s: Exp[String]) extends Def[Long]
  case class LongPlus(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  case class LongLShift(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongURShift(lhs: Exp[Long], rhs: Exp[Int]) extends Def[Long]
  case class LongAnd(lhs: Exp[Long], rhs: Exp[Long]) extends Def[Long]
  
  def obj_long_parse_long(s: Rep[String])(implicit ctx: SourceContext) = ObjLongParseLong(s)
  def long_plus(lhs: Exp[Long], rhs: Exp[Long])(implicit ctx: SourceContext) : Exp[Long] = LongPlus(lhs, rhs)
  def long_lshift(lhs: Exp[Long], rhs: Exp[Int])(implicit ctx: SourceContext) = LongLShift(lhs, rhs)
  def long_and(lhs: Exp[Long], rhs: Exp[Long])(implicit ctx: SourceContext): Exp[Long] = LongAnd(lhs, rhs)
  def long_urshift(lhs: Exp[Long], rhs: Exp[Int])(implicit ctx: SourceContext) = LongURShift(lhs, rhs)
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = ({
    implicit var a: Numeric[A] = null // hack!! need to store it in Def instances??
    e match {
      case LongPlus(x,y) => long_plus(f(x),f(y))
      case LongAnd(x,y) => long_and(f(x), f(y))
      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenLongOps extends ScalaGenBase {
  val IR: LongOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ObjLongParseLong(s) => emitValDef(sym, "java.lang.Long.parseLong(" + quote(s) + ")")
    case LongPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
    case LongLShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
    case LongURShift(lhs,rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))
    case LongAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }
  
  override def quote(x: Exp[Any]): String = x match {
    case Const(l: Long) => l.toString + "L"
    case _ => super.quote(x)
  }
  
}
