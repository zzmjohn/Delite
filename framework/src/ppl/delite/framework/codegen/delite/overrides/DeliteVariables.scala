package ppl.delite.framework.codegen.delite.overrides

import java.io.PrintWriter
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.datastructures.CLikeGenDeliteStruct
import scala.virtualization.lms.internal.{CLikeCodegen, GenerationFailedException}
import scala.virtualization.lms.common._
import scala.collection.mutable.HashSet

trait DeliteScalaGenVariables extends ScalaGenEffect {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val symIsResult = !deliteResult.isEmpty && (deliteResult.get contains sym)
    var gen = false
    
    if (symIsResult) {
      rhs match {
        case NewVar(init) => emitValDef(sym, "new generated.scala.Ref(" + quote(init) + ")"); gen = true
        case _ => // pass
      }
    }

    rhs match {
      case ReadVar(Variable(a)) if deliteInputs.contains(a) => emitValDef(sym, quote(a) + ".get"); gen = true
      case Assign(Variable(a), b) if deliteInputs.contains(a) => emitValDef(sym, quote(a) + ".set(" + quote(b) + ")"); gen = true
      case VarPlusEquals(Variable(a), b) if deliteInputs.contains(a) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get + " + quote(b) + ")"); gen = true
      case VarMinusEquals(Variable(a), b) if deliteInputs.contains(a) => emitValDef(sym, quote(a) + ".set(" + quote(a) + ".get - " + quote(b) + ")"); gen = true
      case _ => // pass
    }

    if (!gen) {
      super.emitNode(sym, rhs)
    }
  }
}

trait DeliteCLikeGenVariables extends CLikeGenEffect {
  val IR: VariablesExp with DeliteOpsExp
  import IR._
 
  // Remove this when GPU targets also use reference types
  protected def reference: String = "->"

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val symIsResult = !deliteResult.isEmpty && (deliteResult.get contains sym)
    var gen = false
    if (symIsResult) {
      rhs match {
        case NewVar(init) => 
          val tpeString = deviceTarget+"Ref"+unwrapSharedPtr(remap(sym.tp))
          if(cppUseSharedPtr)
            stream.println("%s %s(new %s(%s),%sD());".format(wrapSharedPtr(tpeString),quote(sym),tpeString,quote(init),tpeString))
          else
            stream.println("%s *%s = new %s(%s);".format(tpeString,quote(sym),tpeString,quote(init)))
          gen = true
        case _ => // pass
      }
    }

    if (!(deliteInputs intersect syms(rhs)).isEmpty) {
      rhs match {
        case ReadVar(Variable(a)) => emitValDef(sym, quote(a) + reference + "get()"); gen = true
        case Assign(Variable(a), b) => stream.println(quote(a) + reference + "set(" + quote(b) + ");"); gen = true
        case VarPlusEquals(Variable(a), b) => stream.println(quote(a) + reference + "set(" + quote(a) + reference + "get() + " + quote(b) + ");"); gen = true
        case VarMinusEquals(Variable(a), b) => stream.println(quote(a) + reference + "set(" + quote(a) + reference + "get() - " + quote(b) + ");"); gen = true
        case _ => // pass
      }
    }

    if (!gen) {
      super.emitNode(sym, rhs)
    }
  }

  override def getDataStructureHeaders(): String = {
    val out = new StringBuilder
    out.append("#include \"" + deviceTarget + "DeliteVariables.h\"\n")
    if(isAcceleratorTarget) out.append("#include \"Host" + deviceTarget + "DeliteVariables.h\"\n")
    super.getDataStructureHeaders() + out.toString
  }

}

trait DeliteCudaGenVariables extends CudaGenEffect with DeliteCLikeGenVariables {
  override protected def reference: String = "."
}
trait DeliteOpenCLGenVariables extends OpenCLGenEffect with DeliteCLikeGenVariables {
  override protected def reference: String = "."
}

trait DeliteCGenVariables extends CGenEffect with DeliteCLikeGenVariables with CLikeGenDeliteStruct {
  val IR: VariablesExp with DeliteOpsExp
  import IR._

  private val generatedDeliteVariable = HashSet[String]()

  private val deliteVariableString = """
#include <pthread.h>
#include <map>

extern std::map<int,int> *RefCnt;
extern pthread_mutex_t RefCntLock;

class __T__ {
public:
  __TARG__ data;
  int id;

  __T__(__TARG__ _data) {
    data = _data;
    id = 0;
  }

  __TARG__ get(void) {
    return data;
  }

  void set(__TARG__ newVal) {
      data = newVal;
  }    

  void setGlobalRefCnt(int cnt) {
    // need lock?
    printf("setting the global ref count for %d to %d\n",id,cnt);
    pthread_mutex_lock(&RefCntLock);
    std::map<int,int>::iterator it = RefCnt->find(id);
    if(it==RefCnt->end())
      RefCnt->insert(std::pair<int,int>(id,cnt));
    else
      it->second = cnt;
    pthread_mutex_unlock(&RefCntLock);
  }
};

struct __T__D {
  void operator()(__T__ *p) {
    if(p->id == 0) {
      printf("__T__: deleting locally\n");
      //delete p->data;
    }
    else {
      assert(false);
    }
  }
};

"""
  
  override def emitDataStructures(path: String) {
    super.emitDataStructures(path)
    val stream = new PrintWriter(path + deviceTarget + "DeliteVariables.h")
    stream.println("#include \"" + deviceTarget + "DeliteStructs.h\"")
    stream.println("#include \"" + deviceTarget + "DeliteArrays.h\"")
    for(tp <- dsTypesList if(!isVoidType(tp))) {
      emitDeliteVariable(tp, path, stream)
    }
    stream.close()
  }

  private def emitDeliteVariable(m: Manifest[Any], path: String, header: PrintWriter) {
    try {
      val mString = deviceTarget + "Ref" + unwrapSharedPtr(remap(m))
      if(!generatedDeliteVariable.contains(mString)) {
        val stream = new PrintWriter(path + mString + ".h")
        stream.println("#ifndef __" + mString + "__")
        stream.println("#define __" + mString + "__")
        stream.println(deliteVariableString.replaceAll("__T__",mString).replaceAll("__TARG__",remap(m)+addRef(m))) 
        stream.println("#endif")
        stream.close()
        header.println("#include \"" + mString + ".h\"")
        generatedDeliteVariable.add(mString)
      }
    }
    catch {
      case e: GenerationFailedException => //
      case e: Exception => throw(e)  
    }
  }

}