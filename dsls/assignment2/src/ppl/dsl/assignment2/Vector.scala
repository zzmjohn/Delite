package ppl.dsl.assignment2

import virtualization.lms.common.{BaseFatExp, ScalaGenFat, Variables, VariablesExp}
import ppl.delite.framework.DeliteCollection
import java.io.PrintWriter

/**
 * Operations
 */

trait Vector[T] extends DeliteCollection[T]

trait VectorOps extends Variables {
  object Vector {
    //def apply[A:Manifest](length: Int) = vectorNew(unit(length))
    def apply[A:Manifest](length: Rep[Int]) = vectorNew(length)
  }

  //syntax
  def infix_+[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]) = vectorPlus(x,y)
  def infix_+[A](x: Rep[Vector[A]], y: Rep[A])(implicit m: Manifest[A], n: Numeric[A], o: Overloaded1) = vectorPlusScalar(x,y)

  def infix_*[A](x: Rep[Vector[A]], y: Rep[A])(implicit m: Manifest[A], n: Numeric[A], o: Overloaded1) = scalarTimes(x,y)
  def infix_*[A](x: Rep[A], y: Rep[Vector[A]])(implicit m: Manifest[A], n: Numeric[A], o: Overloaded2) = scalarTimes(y,x)

  def infix_length[A](x: Rep[Vector[A]]) = length(x)
  def infix_apply[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int]) = apply(x, idx) //NOTE: unfortunately doesn't currently work with the syntatic sugar for apply
  def infix_update[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int], y: Rep[A]) = update(x, idx, y)
  def infix_pprint[A:Manifest](x: Rep[Vector[A]]) = pprint(x)
  def infix_+=[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]) = addEqual(x,y)

  //operations
  def vectorNew[A:Manifest](length: Rep[Int]): Rep[Vector[A]]
  def vectorPlus[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def vectorPlusScalar[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]
  def scalarTimes[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[A]): Rep[Vector[A]]

  def length[A](x: Rep[Vector[A]]): Rep[Int]
  def apply[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int]): Rep[A]
  def update[A:Manifest](x: Rep[Vector[A]], idx: Rep[Int], y: Rep[A]): Rep[Unit]
  def pprint[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
  def axpy[A:Manifest:Numeric](a: Rep[A], x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
  def addEqual[A:Manifest:Numeric](x: Rep[Vector[A]], y: Rep[Vector[A]]): Rep[Vector[A]]
}

trait VectorOpsExp extends VectorOps with VariablesExp with BaseFatExp {
  this: VectorImplOps with SimpleVectorExp =>

  //implemented via method call on real data structure
  case class VectorNew[A:Manifest](length: Exp[Int]) extends Def[Vector[A]] {
    val mA = manifest[A]
  }

  case class Length[A](x: Exp[Vector[A]]) extends Def[Int]

  case class VectorApply[A:Manifest](x: Exp[Vector[A]], idx: Exp[Int]) extends Def[A]

  case class VectorUpdate[A:Manifest](x: Exp[Vector[A]], idx: Exp[Int], y: Exp[A]) extends Def[Unit]

  //implemented via kernel embedding (sequential)
  case class PPrint[A:Manifest](x: Exp[Vector[A]], print: Exp[Unit])
    extends DeliteOpSingleTask(print)

  //implemented via Delite ops
  case class VectorPlus[A:Manifest:Numeric](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    val alloc = reifyEffects(vectorNew(length(inA)))
    val v = (fresh[A], fresh[A])
    val func = reifyEffects(v._1 + v._2)
  }

  case class VectorPlusScalar[A:Manifest:Numeric](in: Exp[Vector[A]], s: Exp[A])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(vectorNew(length(in)))
    val v = fresh[A]
    val func = reifyEffects(v + s)
  }

  case class ScalarTimes[A:Manifest:Numeric](in: Exp[Vector[A]], s: Exp[A])
    extends DeliteOpMap[A,A,Vector] {

    val alloc = reifyEffects(vectorNew(length(in)))
    val v = fresh[A]
    val func = reifyEffects(v * s)
  }

  case class AXPY[A:Manifest:Numeric](a: Exp[A], inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    val alloc = reifyEffects(vectorNew(length(inA)))
    val v = (fresh[A], fresh[A])
    val func = reifyEffects(a * v._1 + v._2)
  }

  case class AddEqual[A:Manifest:Numeric](inA: Exp[Vector[A]], inB: Exp[Vector[A]])
    extends DeliteOpZipWith[A,A,A,Vector] {

    val alloc = reifyEffects(inA)
    val v = (fresh[A], fresh[A])
    val func = reifyEffects(v._1 + v._2)
  }

  def vectorNew[A:Manifest](length: Exp[Int]) = reflectMutable(VectorNew[A](length))
  def length[A](x: Exp[Vector[A]]) = Length(x)
  def apply[A:Manifest](x: Exp[Vector[A]], idx: Exp[Int]) = VectorApply[A](x, idx)
  def update[A:Manifest](x: Exp[Vector[A]], idx: Exp[Int], y: Exp[A]) = reflectWrite(x)(VectorUpdate[A](x, idx, y))

  def vectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x, y) match {
    case (Def(ScalarTimes(b,a)), c) => printf("[AXPY matched] %s * %s + %s\n", a, b, c); AXPY(a,b,c)
    case _ => VectorPlus(x,y)
  }

  def vectorPlusScalar[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = VectorPlusScalar(x,y)

  def scalarTimes[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[A]) = ScalarTimes(x, y)

  def pprint[A:Manifest](x: Exp[Vector[A]]) = reflectEffect(PPrint(x, reifyEffectsHere(pprint_impl(x))))

  def axpy[A:Manifest:Numeric](a: Exp[A], x: Exp[Vector[A]], y: Exp[Vector[A]]) = AXPY(a,x,y)

  def addEqual[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = reflectWrite(x)(AddEqual(x,y))

}

trait VectorOpsExpOpt extends VectorOpsExp {
  this: VectorImplOps with SimpleVectorExp =>

  override def vectorPlus[A:Manifest:Numeric](x: Exp[Vector[A]], y: Exp[Vector[A]]) = (x,y) match {
    //(aX + aY) = a(X+Y)
    case (Def(ScalarTimes(a,b)), Def(ScalarTimes(c,d))) if (b == d) => scalarTimes(vectorPlus(a,c), b)
    case _ => super.vectorPlus(x,y)
  }
}

trait ScalaGenVectorOps extends ScalaGenFat {
  val IR: VectorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    //these are the ops that call through to the underlying real data structure
    case x@VectorNew(length) => emitValDef(sym, "new generated.scala.VectorImpl[" + remap(x.mA) + "](" + quote(length) + ")")
    case Length(x) => emitValDef(sym, quote(x) + ".length")
    case VectorApply(x, idx) => emitValDef(sym, quote(x) + "(" + quote(idx) + ")")
    case VectorUpdate(x, idx, y) => emitValDef(sym, quote(x) + "(" + quote(idx) + ") = " + quote(y))
    case _ => super.emitNode(sym, rhs)
  }
}

/**
 * Implementation using kernel embedding
 */
trait VectorImplOps { this: SimpleVector =>
  def pprint_impl[A:Manifest](x: Rep[Vector[A]]): Rep[Unit]
}

trait VectorImplOpsStandard extends VectorImplOps {
  this: SimpleVectorCompiler with SimpleVectorLift =>

  def pprint_impl[A:Manifest](x: Rep[Vector[A]]) = {
    print("[ ")
    for (i <- 0 until length(x)) {
      print(x.apply(i)); print(" ")
    }
    print("]\\n")
  }
}
