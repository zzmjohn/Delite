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
  def insertOPProfilingHead(out: StringBuilder, kernelName: String) {
    if(Config.profileEnabled){
      out.append("val task_size = end - idx\n")
      out.append("generated.scala.ProfileTimer.start(\"" + kernelName + "\", false)\n")
    }
  }

  def insertOPProfilingTail(out: StringBuilder, kernelName: String) {
    if(Config.profileEnabled){
      out.append("generated.scala.ProfileTimer.stop(\"" + kernelName + "\", false)\n")
      out.append("val appendMsg = \", size = \" + task_size\n")
      out.append("generated.scala.ProfileTimer.totalTime(\"" + kernelName + "\", appendMsg)\n")
    }    
  }

}

