package ppl.delite.runtime.graph.ops

import ppl.delite.runtime.graph.targets._
import ppl.delite.runtime.graph._


/**
 * Author: Kevin J. Brown
 * Date: Nov 14, 2010
 * Time: 10:12:48 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class OP_Single(val id: String, kernel: String, private[graph] var outputTypesMap: Map[Targets.Value, Map[String,String]], private[graph] var inputTypesMap: Map[Targets.Value, Map[String,String]]) extends OP_Executable {

  final def isDataParallel = false

  override def partition(sym: String) = {
    if (getInputs.isEmpty) Local
    else getInputs.map(i => i._1.partition(i._2)).reduceLeft(_ combine _) //TODO: this could be a dangerous assumption...
  }

  def task = kernel

  def cost = 0
  def size = 0

}
