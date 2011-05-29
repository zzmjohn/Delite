package ppl.dsl.optiml.matrix

import java.io.{PrintWriter}
import ppl.delite.framework.{DeliteApplication, DSLType}
import scala.virtualization.lms.common.DSLOpsExp
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, CGenBase}
import ppl.delite.framework.ops.DeliteOpsExp
import scala.virtualization.lms.internal.{GenerationFailedException}
import ppl.delite.framework.Config
import ppl.dsl.optiml.{OptiMLExp, OptiML}
import ppl.dsl.optiml.datastruct.scala._

/**
 * Author: Bo Wang
 * Date: May 26, 2011
 * Time: 5:05:31 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */


trait MatrixOpsExpOptPrfl extends MatrixOpsExpOpt {
  this: MatrixImplOps with OptiMLExp =>

  case class TimerBegin(e: Def[Any]) extends Def[Unit]
  case class TimerEnd(e: Def[Any]) extends Def[Unit]

  override def matrix_multiply[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = {
    val m = MatrixMultiply(x,y)
    reflectEffect(TimerBegin(m))
    val ret = reflectEffect(m)
    reflectEffect(TimerEnd(m))
    ret
  }

}

trait ScalaGenMatrixOpsPrfl extends ScalaGenBase {
  val IR: MatrixOpsExpOptPrfl
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case TimerBegin(e) if(!Config.profileCoreOps) => {}
    case TimerEnd(e)   if(!Config.profileCoreOps) => {}
    case TimerBegin(e) if(Config.profileCoreOps && e.isInstanceOf[MatrixMultiply[Any]]) => {
      val kernelName = "matrix_multiply"
      emitValDef(sym, "generated.scala.Profiler.startCoreOpTime(\"" + kernelName + "\")")
    }
    case TimerEnd(e)   if(Config.profileCoreOps && e.isInstanceOf[MatrixMultiply[Any]]) => {
      val m = e.asInstanceOf[MatrixMultiply[Any]]
      val kernelName = "matrix_multiply"
      val kernelSize = quote(m.x) + ".numRows*" + quote(m.x) + ".numCols*" + quote(m.y) + ".numCols"
      emitValDef(sym, "generated.scala.Profiler.stopCoreOpTime(\"" + kernelName + "\", " + kernelSize + ")")
    }
    case _ => super.emitNode(sym, rhs)
  }
}

