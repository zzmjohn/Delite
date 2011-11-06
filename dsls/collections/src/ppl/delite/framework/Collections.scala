package ppl.delite.framework.collections



import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

import ppl.delite.framework._
import ppl.delite.framework.codegen._
import ppl.delite.framework.ops._

import ppl.dsl.optila.{OptiLAApplication}
import ppl.dsl.optila.{OptiLAScalaOpsPkg, OptiLAScalaOpsPkgExp, OptiLA, OptiLAExp, OptiLACompiler, OptiLALift}
import ppl.dsl.optila.{OptiLAScalaCodeGenPkg, OptiLACudaCodeGenPkg, OptiLAOpenCLCodeGenPkg, OptiLACCodeGenPkg, OptiLACodeGenBase, OptiLACodeGenScala, OptiLACodeGenCuda, OptiLACodeGenOpenCL, OptiLACodeGenC}

import codegen.delite.overrides._
import codegen.scala.TargetScala
import java.io.File
import java.io.PrintWriter



trait CollectionsOps
extends TraversableOps
with SeqOps
with ArraySeqOps
with MapOps
with HashMap1Ops
with ArraySeqEmitting
with HashMapEmitting
with HashMultiMapEmitting {
self: OptiLA =>
  // def tic(deps: Rep[Any]*) = profile_start(deps)
  // def toc(deps: Rep[Any]*) = profile_stop(deps)

  // def profile_start(deps: Seq[Rep[Any]]): Rep[Unit]
  // def profile_stop(deps: Seq[Rep[Any]]): Rep[Unit]
}


trait CollectionsOpsExp
extends TraversableOpsExp
with SeqOpsExp
with ArraySeqOpsExp
with ArraySeqEmitting
with MapOpsExp
with HashMap1OpsExp
with HashMapEmitting
with HashMultiMapEmitting {
self: OptiLAExp =>
  // case class ProfileStart(deps: List[Exp[Any]]) extends Def[Unit]
  // case class ProfileStop(deps: List[Exp[Any]]) extends Def[Unit]

  // def profile_start(deps: Seq[Exp[Any]]) = reflectEffect(ProfileStart(deps.toList))
  // def profile_stop(deps: Seq[Exp[Any]]) = reflectEffect(ProfileStop(deps.toList))
}


trait ScalaGenCollectionsOps
extends ScalaGenTraversableOps
with ScalaGenSeqOps
with ScalaGenArraySeqOps
with ScalaGenMapOps
with ScalaGenHashMap1Ops
{
  val IR: CollectionsOpsExp
  import IR._
  
  // override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
  //   rhs match {
  //     case ProfileStart(deps) =>
  //       emitValDef(sym, "ppl.delite.runtime.profiler.PerformanceTimer.start(\"app\", true)")
  //     case ProfileStop(deps) =>
  //       emitValDef(sym, "{ ppl.delite.runtime.profiler.PerformanceTimer.stop(\"app\", true); ppl.delite.runtime.profiler.PerformanceTimer.totalTime(\"app\") }")
  //     case _ => super.emitNode(sym, rhs)
  //   }
  // }
}


trait CollScalaOpsPkg extends OptiLAScalaOpsPkg
    with Base
    with ImplicitOps
    //with NumericOps
    with FractionalOps with OrderingOps with StringOps
    with RangeOps with IOOps with ArrayOps with BooleanOps with PrimitiveOps with MiscOps
    with Equal with IfThenElse with Variables with While with TupleOps with ListOps
    with MathOps with CastingOps with ObjectOps
    with SynchronizedArrayBufferOps
    with LongOps


trait CollScalaOpsPkgExp extends OptiLAScalaOpsPkgExp
    with CollScalaOpsPkg
    with ImplicitOpsExp
    //with NumericOpsExp
    with FractionalOpsExp with OrderingOpsExp with StringOpsExp
    with RangeOpsExp with IOOpsExp with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp
    with FunctionsExp with EqualExp with IfThenElseExp with VariablesExp with WhileExp with TupleOpsExp with TupleOpsExpOpt with ListOpsExp
    with DSLOpsExp with MathOpsExp with CastingOpsExp with ObjectOpsExp
    with SynchronizedArrayBufferOpsExp
    with LongOpsExp


trait CollScalaCodeGenPkg extends OptiLAScalaCodeGenPkg
    with ScalaGenImplicitOps
    //with ScalaGenNumericOps
    with ScalaGenFractionalOps with ScalaGenOrderingOps
    with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps with ScalaGenArrayOps with ScalaGenBooleanOps
    with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenFunctions with ScalaGenEqual with ScalaGenIfThenElse
    with ScalaGenVariables with ScalaGenWhile with ScalaGenTupleOps with ScalaGenListOps
    with ScalaGenDSLOps with ScalaGenMathOps with ScalaGenCastingOps
    with ScalaGenObjectOps
    with ScalaGenSynchronizedArrayBufferOps
    with ScalaGenLongOps
    with ScalaGenTupleOpsOpt
{ val IR: CollScalaOpsPkgExp }


trait Collections extends OptiLA with CollScalaOpsPkg with CollectionsOps with TupleOps {
self: OptiLAApplication =>
}


trait CollectionsExp
extends OptiLAExp
with Collections
with CollScalaOpsPkgExp
with CollectionsOpsExp
with TupleOpsExp
with DeliteOpsExp
with VariantsOpsExp
with DeliteAllOverridesExp {
_: DeliteApplication with CollectionsApplication with CollectionsExp =>
  
  override def getCodeGenPkg(t: Target { val IR: CollectionsExp.this.type }): GenericFatCodegen { val IR: CollectionsExp.this.type } = {
    t match {
      case _: TargetScala => new CollectionsCodeGenScala {
        val IR: CollectionsExp.this.type = CollectionsExp.this
      }
      case _ => super.getCodeGenPkg(t) // throw new IllegalArgumentException("unsupported target")
    }
  }
  
}


trait CollectionsCodeGenBase extends OptiLACodeGenBase with GenericFatCodegen with codegen.Utils {
  val IR: DeliteApplication with CollectionsExp
  
  override def initialDefs = IR.deliteGenerator.availableDefs
  
  //def dsmap(s: String) = s.replaceAll("ppl.delite.framework.datastruct.scala", "generated.scala")
  override def dsmap(line: String) : String = {
    val res = line.replaceAll("scala.Tuple2\\[Int, Int\\]", "Long")
    //Console.err.println("rewriting! " + line + " -> " + res)
    super.dsmap(res)
  }
  
  override def remap[A](m: Manifest[A]) = dsmap(super.remap(m))
  
  override def emitDataStructures(path: String) {
    val sep = File.separator
    val relpath = "dsls collections src ppl delite framework collections datastruct".split(" ")
    val dsRoot = Seq(Config.homeDir) ++ relpath ++ Seq(this.toString)
    copyDataStructures(dsRoot.mkString(sep), path, dsmap)
  }
}


trait CollectionsCodeGenScala
extends OptiLACodeGenScala
with CollectionsCodeGenBase
with CollScalaCodeGenPkg
with ScalaGenDeliteOps
with ScalaGenCollectionsOps
with ScalaGenVariantsOps
with ScalaGenDeliteCollectionOps 
with DeliteScalaGenAllOverrides {
  val IR: DeliteApplication with CollectionsExp with DeliteAllOverridesExp
}


trait CollectionsLift extends OptiLALift with LiftAll with LiftVariables with LiftEquals {
_: Collections =>
}


trait CollectionsApplication extends OptiLAApplication with Collections with CollectionsLift


trait CollectionsApplicationRunner extends CollectionsApplication with DeliteApplication with CollectionsExp
