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

object Profiler{
  def emitParallelOPTimerHeader(out: StringBuilder, kernelName: String) {
    if(Config.profileEnabled){
      val showStamp = if(Config.profileShowStamp) "true" else "false"
      out.append("val task_size = end - idx\n")
      out.append("generated.scala.ProfileTimer.startParallelOP(\"" + kernelName + "\", " + showStamp + ")\n")
    }
  }

  def emitParallelTimerTailer(out: StringBuilder, kernelName: String) {
    if(Config.profileEnabled){
      val showStamp = if(Config.profileShowStamp) "true" else "false"
      out.append("generated.scala.ProfileTimer.stopParallelOP(\"" + kernelName + "\", " + showStamp + ")\n")
      if(Config.profileImmediatePrint){
        out.append("val appendMsg = \", size = \" + task_size\n")
        out.append("generated.scala.ProfileTimer.printParallelOPTime(\"" + kernelName + "\", appendMsg)\n")
      }
    }
  }

  def emitKernelTimerHeader(out: StringBuilder, kernelName: String) {
    if(Config.profileEnabled && Config.profileAllKernels){
      out.append("generated.scala.ProfileTimer.startKernel(\"" + kernelName + "\")\n")
    }
  }

  def emitKernelTimerTailer(out: StringBuilder, kernelName: String) {
    if(Config.profileEnabled && Config.profileAllKernels){
      out.append("generated.scala.ProfileTimer.stopKernel(\"" + kernelName + "\")\n")
    }
  }

}

