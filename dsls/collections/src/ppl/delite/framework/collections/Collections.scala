package ppl.delite.framework.collections



import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import ppl.delite.framework._
import ppl.delite.framework.codegen._
import ppl.delite.framework.ops._
import codegen.delite.overrides._
import codegen.scala.TargetScala
import java.io.File



trait CollectionsOps
extends TraversableOps
with SeqOps
with ArraySeqOps


trait CollectionsOpsExp
extends TraversableOpsExp
with SeqOpsExp
with ArraySeqOpsExp


trait ScalaGenCollectionsOps
extends ScalaGenTraversableOps
with ScalaGenSeqOps
with ScalaGenArraySeqOps
{
  val IR: CollectionsOpsExp
}


trait Collections extends ScalaOpsPkg with CollectionsOps


trait CollectionsExp
extends Collections
with ScalaOpsPkgExp
with CollectionsOpsExp
with DeliteOpsExp
with VariantsOpsExp
with DeliteAllOverridesExp {
_: DeliteApplication with CollectionsApplication with CollectionsExp =>
  
  def getCodeGenPkg(t: Target { val IR: CollectionsExp.this.type }): GenericFatCodegen { val IR: CollectionsExp.this.type } = {
    t match {
      case _: TargetScala => new CollectionsCodeGenScala {
        val IR: CollectionsExp.this.type = CollectionsExp.this
      }
      case _ => throw new IllegalArgumentException("unsupported target")
    }
  }
  
}


trait CollectionsCodeGenBase extends GenericFatCodegen with codegen.Utils {
  val IR: DeliteApplication with CollectionsExp
  
  override def initialDefs = IR.deliteGenerator.availableDefs
  
  def dsmap(s: String) = s
  
  override def remap[A](m: Manifest[A]) = dsmap(super.remap(m))
  
  override def emitDataStructures(path: String) {
    val sep = File.separator
    val relpath = "dsls collections src ppl delite framework collections datastruct".split(" ")
    val dsRoot = Seq(Config.homeDir) ++ relpath ++ Seq(this.toString)
    copyDataStructures(dsRoot.mkString(sep), path, dsmap)
  }
}


trait CollectionsCodeGenScala
extends CollectionsCodeGenBase
with ScalaCodeGenPkg
with ScalaGenDeliteOps
with ScalaGenCollectionsOps
with ScalaGenVariantsOps
with ScalaGenDeliteCollectionOps 
with DeliteScalaGenAllOverrides {
  val IR: DeliteApplication with CollectionsExp with DeliteAllOverridesExp
}


trait CollectionsLift extends LiftScala {
_: Collections =>
}


trait CollectionsApplication extends Collections with CollectionsLift


trait CollectionsApplicationRunner extends CollectionsApplication with DeliteApplication with CollectionsExp
