package ppl.delite.runtime.codegen

import xml.XML
import ppl.delite.runtime.Config
import ppl.delite.runtime.graph.targets.Targets
import tools.nsc.io._

object CppCompile extends CCompile {

  def target = Targets.Cpp
  override def ext = "cpp"

  protected def configFile = "CPP.xml"
  protected def compileFlags = Array("-w", "-O3", "-shared", "-fPIC", "-std=c++0x")
  protected def outputSwitch = "-o"
  
  private val dsFiles = Directory(Path(sourceCacheHome + "datastructures")).files.toList
  //raghu
  override protected def auxSourceList = dsFiles.filter(_.extension == ext).map(_.toAbsolute.toString) ++ List(sourceCacheHome + "kernels" + sep + target + "helperFuncs." + ext, Config.deliteHome + sep + "runtime" + sep + target + sep + "DeliteCppStringHelpers." + ext)

}
