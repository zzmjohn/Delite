package ppl.delite.runtime.codegen

import ppl.delite.runtime.Config

/**
 * Author: Bo Wang
 * Date: May 22, 2011
 * Time: 7:39:08 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object ProfileGenerator{

  val showStamp = if(Config.profileShowStamp) "true" else "false"

  def emitParallelOpTimerHeader(out: StringBuilder, kernelId: String, chunkId: Int, kernelType: String) {
    if(Config.profileEnabled){
      out.append("val kernelSize = end - idx\n")
      out.append("generated.scala.Profiler.recordOpType(\"" + kernelId + "\", \"" + kernelType + "\")\n")
      out.append("generated.scala.Profiler.startParallelOp(\"" + kernelId + "\", " + chunkId + ", " + showStamp + ")\n")
    }
  }

  def emitParallelOpTimerTailer(out: StringBuilder, kernelId: String, chunkId: Int) {
    if(Config.profileEnabled){
      out.append("generated.scala.Profiler.stopParallelOp(\"" + kernelId + "\", " + chunkId + ", kernelSize, " + showStamp + ")\n")
    }
  }

  def emitKernelTimerHeader(out: StringBuilder, kernelId: String) {
    if(Config.profileEnabled && Config.profileAllKernels){
      out.append("generated.scala.Profiler.recordOpType(\"" + kernelId + "\", \"SingleTask\")\n")
      out.append("generated.scala.Profiler.startSequentialOp(\"" + kernelId + "\")\n")
    }
  }

  def emitKernelTimerTailer(out: StringBuilder, kernelId: String) {
    if(Config.profileEnabled && Config.profileAllKernels){
      out.append("generated.scala.Profiler.stopSequentialOp(\"" + kernelId + "\")\n")
    }
  }

}

