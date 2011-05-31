package ppl.dsl.optiml.vector

import java.io.{PrintWriter}
import ppl.delite.framework.Config
import ppl.dsl.optiml.datastruct.scala._
import ppl.dsl.optiml.{ProfileOps, OptiMLExp, OptiML}
import scala.virtualization.lms.common.{ScalaGenFat, ScalaGenBase}

/**
 * Author: Bo Wang
 * Date: May 31, 2011
 * Time: 9:24:52 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait VectorOpsExpPrfl extends VectorOpsExpOpt with ProfileOps {
  this: VectorImplOps with OptiMLExp =>

  override def vector_outer[A:Manifest:Arith](x: Exp[Vector[A]], y: Exp[Vector[A]]) = {
    val v = VectorOuter(x,y)
    reflectEffect(TimerBegin(v))
    val ret = reflectEffect(v)
    reflectEffect(TimerEnd(v))
    ret
  }

}

trait ScalaGenVectorOpsPrfl extends BaseGenVectorOps with ScalaGenFat {
  val IR: VectorOpsExpPrfl
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case TimerBegin(e) if(!Config.profileCoreOps) => {}
    case TimerEnd(e)   if(!Config.profileCoreOps) => {}
    case TimerBegin(e) if(Config.profileCoreOps && e.isInstanceOf[VectorOuter[Any]]) => {
      val kernelName = "vector_outer"
      emitValDef(sym, "generated.scala.Profiler.startCoreOpTime(\"" + kernelName + "\")")
    }
    case TimerEnd(e)   if(Config.profileCoreOps && e.isInstanceOf[VectorOuter[Any]]) => {
      val m = e.asInstanceOf[VectorOuter[Any]]
      val kernelName = "vector_outer"
      val kernelSize = quote(m.x) + ".length*" + quote(m.y) + ".length"
      emitValDef(sym, "generated.scala.Profiler.stopCoreOpTime(\"" + kernelName + "\", " + kernelSize + ")")
    }
    case _ => super.emitNode(sym, rhs)
  }
}

