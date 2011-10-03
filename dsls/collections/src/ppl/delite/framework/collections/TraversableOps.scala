package ppl.delite.framework.collections



import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common._
import ppl.delite.framework.collections.datastruct.scala._
import ppl.delite.framework.ops.DeliteOpsExp



trait TraversableOps extends DSLType with Variables {
  
  implicit def rep2traversableops[T: Manifest](t: Rep[Traversable[T]]) = new TraversableClsOps(t)
  
  class TraversableClsOps[T: Manifest](t: Rep[Traversable[T]]) {
    def size: Rep[Int] = traversable_size(t)
    def foreach(block: Rep[T] => Rep[Unit]) = traversable_foreach(t, block)
  }
  
  /* class defs */
  def traversable_size[T: Manifest](t: Rep[Traversable[T]]): Rep[Int]
  def traversable_foreach[T: Manifest](t: Rep[Traversable[T]], block: Rep[T] => Rep[Unit]): Rep[Unit]
  
}


trait TraversableOpsExp extends TraversableOps with VariablesExp with BaseFatExp with DeliteOpsExp {
  
  /* nodes */
  case class TraversableSize[T: Manifest](t: Exp[Traversable[T]]) extends Def[Int]
  case class TraversableForeach[T:Manifest](in: Exp[Traversable[T]], func: Exp[T] => Exp[Unit])
  extends DeliteOpForeach[T] {
    def sync = n => Const(List()) // ? why not: n => List() - where's the implicit to do this??
    val size = copyTransformedOrElse(_.size)(in.size)
  }
  
  /* class interface */
  def traversable_size[T: Manifest](t: Exp[Traversable[T]]) = reflectPure(TraversableSize(t))
  def traversable_foreach[T: Manifest](t: Exp[Traversable[T]], block: Exp[T] => Rep[Unit]) = reflectEffect(TraversableForeach(t, block))
  
}


trait ScalaGenTraversableOps extends ScalaGenFat {
  val IR: TraversableOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // these are the ops that call through to the underlying real data structure
    case TraversableSize(t) => emitValDef(sym, quote(t) + ".size")
    case _ => super.emitNode(sym, rhs)
  }
  
}


