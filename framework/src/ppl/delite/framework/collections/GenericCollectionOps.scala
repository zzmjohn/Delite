package ppl.delite.framework.collections



import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.virtualization.lms.common._
import ppl.delite.framework.collections.datastruct.scala._
import ppl.delite.framework.ops.DeliteOpsExp



trait GenericCollectionOps extends DSLType with Variables {
  
  /** Definition of the CanBuild evidence */
  trait CanBuild[-Coll, -S, +Target] {
    def alloc(source: Rep[Coll]): Rep[Target]
    def emitterProvider(source: Rep[Coll]): EmitterProvider
  }
  
}


trait GenericCollectionGen {
}


trait EmitterProvider {
  def emitterScala: Emitter
}


/** Collection construction emitter. Given that the collection can be emitted for a given
 *  platform, uses the evidence to emit the code.
 */
trait Emitter {
  def emitBufferDefs(basename: String, elemtype: String)(implicit stream: PrintWriter)
  def emitInitSubActivation(basename: String, activname: String)(implicit stream: PrintWriter)
  def emitAddToBuffer(basename: String, activname: String, elemname: String)(implicit stream: PrintWriter)
  def emitPostCombine(basename: String, activname: String, lhsname: String)(implicit stream: PrintWriter)
  def emitPostProcInit(basename: String, activname: String)(implicit stream: PrintWriter)
  def emitPostProcess(basename: String, activname: String)(implicit stream: PrintWriter)
}


trait ScalaTarget
trait CudaTarget
trait CTarget
