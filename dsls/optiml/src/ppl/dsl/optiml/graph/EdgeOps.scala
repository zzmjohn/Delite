package ppl.dsl.optiml.graph

import ppl.dsl.optiml.datastruct.CudaGenDataStruct
import ppl.dsl.optiml.datastruct.scala._
import java.io.{PrintWriter}

import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.ops.DeliteOpsExp
import reflect.Manifest
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenerationFailedException, GenericNestedCodegen, CGenBase, CudaGenBase, ScalaGenBase}
import ppl.dsl.optiml.{OptiMLExp, OptiML}

trait EdgeOps extends DSLType with Variables {
  this: OptiML =>

  // base class edges cannot be instantiated at the moment

//  implicit def repEdgeToEdgeOps(e: Rep[Edge]) = new edgeOpsCls(e)
//
//  class edgeOpsCls(e: Rep[Edge]) {
//  }

  // class defs
}

trait EdgeOpsExp extends EdgeOps with EffectExp {

  this: OptiMLExp =>

  ///////////////////////////////////////////////////
  // implemented via method on real data structure


  /////////////////////
  // class interface

}


trait BaseGenEdgeOps extends GenericNestedCodegen {
  val IR: EdgeOpsExp
  import IR._

  //override def syms(e: Any): List[Sym[Any]] = e match {
    //case _ => super.syms(e)
  //}
}

trait ScalaGenEdgeOps extends BaseGenEdgeOps with ScalaGenBase {
  val IR: EdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
    rhs match {
      case _ => super.emitNode(sym, rhs)
    }
  }
}


trait CudaGenEdgeOps extends BaseGenEdgeOps with CudaGenBase with CudaGenDataStruct {
  val IR: EdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenEdgeOps extends BaseGenEdgeOps with CGenBase {
  val IR: EdgeOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }
}