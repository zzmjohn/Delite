package ppl.delite.framework.ops

import scala.virtualization.lms.common._
import scala.reflect.{SourceContext, RefinedManifest}
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Config

trait DeliteFileReaderOps extends Base with DeliteArrayBufferOps {
  object DeliteFileReader {
    def readLines[A:Manifest](path: Rep[String])(f: Rep[String] => Rep[A]): Rep[DeliteArray[A]] = dfr_readLines(path, (line, buf) => buf += f(line))
    def readLinesUnstructured[A:Manifest](path: Rep[String])(f: (Rep[String], Rep[DeliteArrayBuffer[A]]) => Rep[Unit]): Rep[DeliteArray[A]] = dfr_readLines(path, f)
  }

  def dfr_readLines[A:Manifest](path: Rep[String], f: (Rep[String], Rep[DeliteArrayBuffer[A]]) => Rep[Unit]): Rep[DeliteArray[A]]
  
}

trait DeliteFileReaderOpsExp extends DeliteFileReaderOps with DeliteArrayOpsExpOpt with DeliteArrayBufferOpsExp with DeliteOpsExp {

  def dfr_readLines[A:Manifest](path: Rep[String], f: (Rep[String], Rep[DeliteArrayBuffer[A]]) => Rep[Unit]) = reflectPure(DeliteOpFileReaderReadLines(reifyEffects(path), f))
  case class DeliteOpFileReaderReadLines[A:Manifest](path: Block[String], func: (Rep[String], Rep[DeliteArrayBuffer[A]]) => Rep[Unit]) extends DeliteOpInput[DeliteArray[A]] {
    type OpType <: DeliteOpFileReaderReadLines[A]
    val mA = manifest[A]

    val line: Sym[String] = copyTransformedOrElse(_.line)(fresh[String]).asInstanceOf[Sym[String]]
    val allocVal: Sym[DeliteArrayBuffer[A]] = copyTransformedOrElse(_.allocVal)(reflectMutableSym(fresh[DeliteArrayBuffer[A]])).asInstanceOf[Sym[DeliteArrayBuffer[A]]]
    val alloc: Block[DeliteArrayBuffer[A]] = copyTransformedBlockOrElse(_.alloc)(reifyEffects(DeliteArrayBuffer[A]()))
    val append: Block[Unit] = copyTransformedBlockOrElse(_.append)(reifyEffects(func(line, allocVal)))
    val finalizer: Block[DeliteArray[A]] = copyTransformedBlockOrElse(_.finalizer)(reifyEffects{ darray_buffer_raw_data(allocVal).take(allocVal.length) })
  }

  override def blocks(e: Any): List[Block[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.blocks(i) ::: blocks(i.alloc) ::: blocks(i.append) ::: blocks(i.finalizer)
    case _ => super.blocks(e)
  }  
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.syms(i) ::: syms(i.alloc) ::: syms(i.append) ::: syms(i.finalizer)
    case _ => super.syms(e)
  }
    
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.readSyms(i) ::: readSyms(i.alloc) ::: readSyms(i.append) ::: readSyms(i.finalizer)
    case _ => super.readSyms(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.boundSyms(i) ::: effectSyms(i.alloc) ::: effectSyms(i.append) ::: syms(i.line) ::: syms(i.allocVal) ::: effectSyms(i.finalizer)
    case _ => super.boundSyms(e)
  }
  
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case i: DeliteOpFileReaderReadLines[_] => super.symsFreq(i) ::: freqNormal(i.alloc) ::: freqHot(i.append) ::: freqNormal(i.finalizer)
    case _ => super.symsFreq(e)
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => Nil    
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case i: DeliteOpFileReaderReadLines[_] => syms(i.alloc)
    case _ => super.copySyms(e)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@DeliteOpFileReaderReadLines(path,func) => reflectPure(new { override val original = Some(f,e) } with DeliteOpFileReaderReadLines(f(path),f(func))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])      
    case Reflect(e@DeliteOpFileReaderReadLines(path,func), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with DeliteOpFileReaderReadLines(f(path),f(func))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait ScalaGenDeliteFileReaderOps extends ScalaGenFat {
  val IR: DeliteFileReaderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

    case op: DeliteOpFileReaderReadLines[_] => //TODO: how much of these file reader impl wrappers do we want in the runtime?
            emitBlock(op.path)
      emitValDef(sym, "{")
        stream.println("val raghuFile = /*raghu in delite - DeliteFileReader.scala*/ new java.io.File(" + quote(getBlockResult(op.path)) + ")")
        stream.println("val (start, end) = ppl.delite.runtime.DeliteMesosExecutor.getBlockSize(raghuFile)")
        stream.println("val raghuInput = /*raghu in delite - DeliteFileReader.scala*/ new java.io.BufferedReader(new java.io.FileReader(raghuFile))")
       stream.println("var pos = start")
        stream.println("if (pos != 0) {")
          stream.println("raghuInput.skip(pos-1)")
          stream.println("pos += raghuInput.readLine().length") //+1-1
        stream.println("}")

        emitBlock(op.alloc)
        emitValDef(op.allocVal, quote(getBlockResult(op.alloc)))

        stream.println("var " + quote(op.line) + " = raghuInput.readLine()")
        stream.println("while(pos < end && " + quote(op.line) + " != null) {")
          emitBlock(op.append)
          stream.println("pos += " + quote(op.line) + ".length + 1") //TODO: could be 1 or 2 characters extra
          stream.println(quote(op.line) + " = raghuInput.readLine()")
        stream.println("}")
        stream.println("raghuInput.close()")
        emitBlock(op.finalizer)
        stream.println("val act = new activation_" + quote(sym))
        stream.println("act." + quote(sym) + " = " + quote(getBlockResult(op.finalizer)))
        stream.println("act")
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }

  override def emitNodeKernelExtra(syms: List[Sym[Any]], rhs: Def[Any]): Unit = rhs match {
    case op: DeliteOpFileReaderReadLines[_] => //TODO: use activation record / closure style like MultiLoop
//      throw new Exception("emitNodeKernelExtra called")
      val sym = syms(0)
      val actType = "activation_" + quote(sym)
      stream.println("final class " + actType + " {")
        stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = _")
        
        if (Config.generateSerializable) {
          stream.println("def combine(act: " + actType + ", rhs: " + actType + ") {")
          val tpe = remap(sym.tp)
          val obj = if (tpe.contains("DeliteArrayObject")) tpe.take(tpe.indexOf("[")) else tpe
          stream.println("act." + quote(sym) + " = " + obj + ".combine(act." + quote(sym) + "," + "rhs." + quote(sym) + ")")
          stream.println("}")

          stream.println("def serialize(): java.util.ArrayList[com.google.protobuf.ByteString] = {")
            stream.println("val arr = new java.util.ArrayList[com.google.protobuf.ByteString]")
            stream.println("arr.add(ppl.delite.runtime.messages.Serialization.serialize(this." + quote(sym) + ", true, \"" + quote(sym) + "\"))")
            stream.println("arr")
          stream.println("}")
        }
        stream.println("}")

        if (Config.generateSerializable) {
          stream.println("object " + actType + " {")
            stream.println("def deserialize(bytes: java.util.List[com.google.protobuf.ByteString]) = {")
              stream.println("val act = new " + actType)
              stream.println("act." + quote(sym) + " = ppl.delite.runtime.messages.Serialization.deserialize(classOf[" + remap(sym.tp) + "], bytes.get(0))")
              stream.println("act")
            stream.println("}")
          stream.println("}")
      }
    case _ => super.emitNodeKernelExtra(syms, rhs)
  }

  /* def dfr_readLines_impl_cluster = {
    //Delite runtime magic to produce config... (& blocks?)
    val path = Path(filePath)
    val fs = FileSystem.get(config)
    val status = fs.getFileStatus(path)
    val blocks = fs.getFileBlockLocations(fileStatus, 0, status.getLen)
    for (block <- blocks if block.getHosts contains myHostName) {
      val start = block.getOffset
      val end = start + block.getLength
      //do remainder within loop ... concatenate blocks? are they contiguous?

    val file = fs.open(path) //in or out of loop? ...
    if (start != 0) {
      file.seek(start) //TODO: adjust for broken lines
    }
    val input = LineReader(file)
    val buffer = emitAlloc()
    var line = Text()
    var pos = start
    var lineSize = input.readLine(line)
    while (pos < end && lineSize > 0) {
      val lineSym = line.toString
      emitAppend()
      line = Text()
      pos += lineSize
      lineSize = input.readLine(line)
    }
    input.close()
    buffer
  } }*/

}

/*raghu - experimental*/
trait CGenDeliteFileReaderOps extends CGenFat {
  override val IR: DeliteFileReaderOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: DeliteOpFileReaderReadLines[_] => //TODO: how much of these file reader impl wrappers do we want in the runtime?
      //      emitBlock(op.path)
      // emitValDef(sym, "{")

        stream.println("std::ifstream raghuFile(" + quote(getBlockResult(op.path)) + ");")
        stream.println("if (!raghuFile.is_open()) {")
        stream.println("std::cerr << \"Unable to open \" <<  " + quote(getBlockResult(op.path)) + "<< std::endl;")
        stream.println("exit(-1);")
        stream.println("}")
        // The DeliteMesosExecutor part doesn't really do anything apparently, so leaving
        // that logic out in the cpp code generator. If you don't know what I'm talking 
        // about, look at the Scala code generator for the same op.
       stream.println("int pos = 0;")
        emitBlock(op.alloc)
        emitValDef(op.allocVal, quote(getBlockResult(op.alloc)))

        // Explicitly using std::string so that it doesn't get clobbered
        // by whatever the 'string' keyword is typedef'ed to
        stream.println("std::string tempStringToReadALineUntilStringsAreImplementedCorrectly;")
        stream.println("charAsString* " + quote(op.line) + "=0;")
        stream.println("getline(raghuFile, tempStringToReadALineUntilStringsAreImplementedCorrectly);")
        stream.println(quote(op.line) + " = (charAsString*)tempStringToReadALineUntilStringsAreImplementedCorrectly.c_str();")
        stream.println("while(tempStringToReadALineUntilStringsAreImplementedCorrectly.length() > 0) {")
        emitBlock(op.append)
        stream.println("pos += tempStringToReadALineUntilStringsAreImplementedCorrectly.length() + 1;") //TODO: could be 1 or 2 characters extra
        stream.println("getline(raghuFile, tempStringToReadALineUntilStringsAreImplementedCorrectly);")
        stream.println(quote(op.line) + " = (charAsString*)tempStringToReadALineUntilStringsAreImplementedCorrectly.c_str();")


        stream.println("}")
        stream.println("raghuFile.close();")
        emitBlock(op.finalizer)
        stream.println("activation_"+quote(sym)+" *"+quote(sym)+" =  new activation_" + quote(sym) + ";")
        stream.println(quote(sym)+"->" + quote(sym) + " = " + quote(getBlockResult(op.finalizer)) + ";")
//        stream.println("act")

//        emitBlock(op.path)
//        emitValDef(sym, quote(getBlockResult(op.finalizer)))

    case _ => super.emitNode(sym, rhs)
  }

  override def emitNodeKernelExtra(syms: List[Sym[Any]], rhs: Def[Any]): Unit = rhs match {
    case op: DeliteOpFileReaderReadLines[_] => //TODO: use activation record / closure style like MultiLoop
//      throw new Exception("emitNodeKernelExtra called")
      val sym = syms(0)
      val actType = "activation_" + quote(sym)
        typesStream.println("class " + actType + " {")
        typesStream.println("public:")
        typesStream.println(remap(sym.tp) + " " + "*"+quote(sym) + ";")
        typesStream.println(actType + "()")
        typesStream.println("{")
        typesStream.println(quote(sym) + " =  0;")
        typesStream.println("}")

        
        if (Config.generateSerializable) {
          throw new Exception("Not implemented yet")
        }
        typesStream.println("};")

        if (Config.generateSerializable) {
          throw new Exception("Not implemented yet")
      }
    case _ => 
//    throw new Exception("emitNodeKernelExtra called")
      super.emitNodeKernelExtra(syms, rhs)
  }
/*
  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean,
    external: Boolean): Unit = {
    //TODO: Remove the dependency to Multiloop to Delite
    if (resultType.startsWith("activation_")) {
      stream.println("return act;")
      stream.println("}")
      dsTypesList ++= (syms++vals++vars).map(_.tp)
    }
    else {
      super.emitKernelFooter(syms, vals, vars, resultType, resultIsVar, external)
    }
  }
  */
}
