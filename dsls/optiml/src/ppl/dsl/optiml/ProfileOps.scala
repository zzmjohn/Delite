package ppl.dsl.optiml


/**
 * Author: Bo Wang
 * Date: May 31, 2011
 * Time: 12:31:06 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

trait ProfileOps {
  this: OptiMLExp =>

  case class TimerBegin(e: Def[Any]) extends Def[Unit]
  case class TimerEnd(e: Def[Any]) extends Def[Unit]

}

