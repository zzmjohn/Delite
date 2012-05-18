package ppl.delite.framework.extern

import _root_.scala.virtualization.lms.internal._
import collection.mutable.{ListBuffer, HashMap, HashSet}
import java.io.{FileWriter, BufferedWriter, File, PrintWriter}

import ppl.delite.framework.{Config, DeliteApplication}
import ppl.delite.framework.extern.lib._
import ppl.delite.framework.ops._
import ppl.delite.framework.codegen.delite._

trait DeliteGenExternal extends DeliteCodegen {
  val IR: DeliteOpsExp
  import IR._

  val generatedOps = HashSet[String]()

  // small hack in order to bypass removal of ThinDef in commit d9f33ea57ac11031511a80009a621148747d7657
  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef): Unit = emitFatNodeAny(sym, rhs)

  private def emitFatNodeAny(sym: List[Sym[Any]], rhs: AnyRef): Unit = rhs match {
    case e:DeliteOpExternal[_] if !generatedOps.contains(e.funcName) => 
      var foundTarget = false
      for (g <- generators) {
        try{
           g.emitExternalLib(e)
           foundTarget = true
        }
        catch {
          case g:GenerationFailedException => 
        }
      }
      if (!foundTarget) throw new GenerationFailedException("No generator could be found for external lib: " + e)

      generatedOps += e.funcName
      super.emitFatNode(sym, rhs.asInstanceOf[FatDef]) // pass on to DeliteGenTaskGraph
      
    case _ => super.emitFatNode(sym, rhs.asInstanceOf[FatDef])
  }
  
}
