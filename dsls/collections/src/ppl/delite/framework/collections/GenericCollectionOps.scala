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
    def emitter(source: Rep[Coll]): Emitter[Target]
  }
  
  /** Denotes whether the collection can be generated for a given platform. */
  trait CanEmit[-Coll, HardwareTarget] {
    def emitBufferDefs(basename: String)(implicit stream: PrintWriter)
    def emitInitSubActivation(basename: String, activname: String)(implicit stream: PrintWriter)
    def emitAddToBuffer(basename: String, activname: String)(implicit stream: PrintWriter)
    def emitPostCombine(basename: String, activname: String, lhsname: String)(implicit stream: PrintWriter)
    def emitPostProcInit(basename: String, activname: String)(implicit stream: PrintWriter)
    def emitPostProcess(basename: String, activname: String)(implicit stream: PrintWriter)
  }
  
  /** Collection construction emitter. Given that the collection can be emitted for a given
   *  platform, uses the evidence to emit the code.
   */
  trait Emitter[+Coll] {
    def emitBufferDefs[H](basename: String)(implicit stream: PrintWriter, ce: CanEmit[Coll, H]) = ce.emitBufferDefs(basename)
    def emitInitSubActivation[H](basename: String, activname: String)(implicit stream: PrintWriter, ce: CanEmit[Coll, H]) = ce.emitInitSubActivation(basename, activname)
    def emitAddToBuffer[H](basename: String, activname: String)(implicit stream: PrintWriter, ce: CanEmit[Coll, H]) = ce.emitAddToBuffer(basename, activname)
    def emitPostCombine[H](basename: String, activname: String, lhsname: String)(implicit stream: PrintWriter, ce: CanEmit[Coll, H]) = ce.emitPostCombine(basename, activname, lhsname)
    def emitPostProcInit[H](basename: String, activname: String)(implicit stream: PrintWriter, ce: CanEmit[Coll, H]) = ce.emitPostProcInit(basename, activname)
    def emitPostProcess[H](basename: String, activname: String)(implicit stream: PrintWriter, ce: CanEmit[Coll, H]) = ce.emitPostProcess(basename, activname)
  }
  
}


trait GenericCollectionGen {
  trait ScalaTarget
  trait CudaTarget
  trait CTarget
}


