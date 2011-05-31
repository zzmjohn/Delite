package ppl.dsl.optiml.matrix

import java.io.{PrintWriter}
import scala.virtualization.lms.common.ScalaGenBase
import ppl.delite.framework.Config
import ppl.dsl.optiml.datastruct.scala._
import ppl.dsl.optiml.{ProfileOps, OptiMLExp, OptiML}

/**
 * Author: Bo Wang
 * Date: May 26, 2011
 * Time: 5:05:31 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */


trait MatrixOpsExpPrfl extends MatrixOpsExpOpt with ProfileOps {
  this: MatrixImplOps with OptiMLExp =>

  override def matrix_multiply[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = {
    val m = MatrixMultiply(x,y)
    reflectEffect(TimerBegin(m))
    val ret = reflectEffect(m)
    reflectEffect(TimerEnd(m))
    ret
  }

  override def matrix_times[A:Manifest:Arith](x: Exp[Matrix[A]], y: Exp[Matrix[A]]) = {
    val m = MatrixTimes(x,y)
    reflectEffect(TimerBegin(m))
    val ret = reflectEffect(m)
    reflectEffect(TimerEnd(m))
    ret
  }

  override def matrix_inverse[A](x: Exp[Matrix[A]])(implicit mA: Manifest[A], conv: Exp[A] => Exp[Double]) = {
    val m = MatrixInverse(x)
    reflectEffect(TimerBegin(m))
    val ret = reflectEffect(m)
    reflectEffect(TimerEnd(m))
    ret
  }

  override def matrix_transpose[A:Manifest](x: Exp[Matrix[A]]) = {
    val m = MatrixTranspose(x)
    reflectEffect(TimerBegin(m))
    val ret = reflectEffect(m)
    reflectEffect(TimerEnd(m))
    ret
  }

}

trait ScalaGenMatrixOpsPrfl extends ScalaGenBase {
  val IR: MatrixOpsExpPrfl
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    // TimerBegin
    case TimerBegin(e) if(!Config.profileCoreOps) => {}
    case TimerBegin(e) if(Config.profileCoreOps && e.isInstanceOf[MatrixMultiply[Any]]) => {
      val kernelName = "matrix_multiply"
      emitValDef(sym, "generated.scala.Profiler.startCoreOpTime(\"" + kernelName + "\")")
    }
    case TimerBegin(e) if(Config.profileCoreOps && e.isInstanceOf[MatrixTimes[Any]]) => {
      val kernelName = "matrix_times"
      emitValDef(sym, "generated.scala.Profiler.startCoreOpTime(\"" + kernelName + "\")")
    }
    case TimerBegin(e) if(Config.profileCoreOps && e.isInstanceOf[MatrixInverse[Any]]) => {
      val kernelName = "matrix_inverse"
      emitValDef(sym, "generated.scala.Profiler.startCoreOpTime(\"" + kernelName + "\")")
    }
    case TimerBegin(e) if(Config.profileCoreOps && e.isInstanceOf[MatrixTranspose[Any]]) => {
      val kernelName = "matrix_transpose"
      emitValDef(sym, "generated.scala.Profiler.startCoreOpTime(\"" + kernelName + "\")")
    }

    // Timer End
    case TimerEnd(e)   if(!Config.profileCoreOps) => {}
    case TimerEnd(e)   if(Config.profileCoreOps && e.isInstanceOf[MatrixMultiply[Any]]) => {
      val m = e.asInstanceOf[MatrixMultiply[Any]]
      val kernelName = "matrix_multiply"
      val kernelSize = quote(m.x) + ".numRows*" + quote(m.x) + ".numCols*" + quote(m.y) + ".numCols"
      emitValDef(sym, "generated.scala.Profiler.stopCoreOpTime(\"" + kernelName + "\", " + kernelSize + ")")
    }
    case TimerEnd(e)   if(Config.profileCoreOps && e.isInstanceOf[MatrixTimes[Any]]) => {
      val m = e.asInstanceOf[MatrixTimes[Any]]
      val kernelName = "matrix_times"
      val kernelSize = quote(m.inA) + ".numRows*" + quote(m.inA) + ".numCols"
      emitValDef(sym, "generated.scala.Profiler.stopCoreOpTime(\"" + kernelName + "\", " + kernelSize + ")")
    }
    case TimerEnd(e)   if(Config.profileCoreOps && e.isInstanceOf[MatrixInverse[Any]]) => {
      val m = e.asInstanceOf[MatrixInverse[Any]]
      val kernelName = "matrix_inverse"
      val kernelSize = quote(m.x) + ".numRows*" + quote(m.x) + ".numCols"
      emitValDef(sym, "generated.scala.Profiler.stopCoreOpTime(\"" + kernelName + "\", " + kernelSize + ")")
    }
    case TimerEnd(e)   if(Config.profileCoreOps && e.isInstanceOf[MatrixTranspose[Any]]) => {
      val m = e.asInstanceOf[MatrixTranspose[Any]]
      val kernelName = "matrix_transpose"
      val kernelSize = quote(m.x) + ".numRows*" + quote(m.x) + ".numCols"
      emitValDef(sym, "generated.scala.Profiler.stopCoreOpTime(\"" + kernelName + "\", " + kernelSize + ")")
    }

    case _ => super.emitNode(sym, rhs)
  }
}


