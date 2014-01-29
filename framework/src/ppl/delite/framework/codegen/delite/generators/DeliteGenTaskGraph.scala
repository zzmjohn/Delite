package ppl.delite.framework.codegen.delite.generators

import collection.mutable.{ArrayBuffer, ListBuffer, HashMap}
import java.io.{StringWriter, FileWriter, File, PrintWriter}
import scala.virtualization.lms.common.LoopFusionOpt
import scala.virtualization.lms.internal.{GenericCodegen, CLikeCodegen, ScalaCodegen, GenerationFailedException}
import scala.virtualization.lms.internal.Targets._
import ppl.delite.framework.ops.DeliteCollection
import scala.reflect.SourceContext

import ppl.delite.framework.codegen.delite.DeliteCodegen
import ppl.delite.framework.ops._
import ppl.delite.framework.Config
import ppl.delite.framework.transform.LoopSoAOpt
import ppl.delite.framework.datastructures.DeliteArray
import ppl.delite.framework.analysis.{LoopAnalysis,StencilAnalysis}
import ppl.delite.framework.datastructures.DeliteArrayFatExp

trait DeliteGenTaskGraph extends DeliteCodegen with LoopFusionOpt with LoopSoAOpt {
  val IR: DeliteOpsExp
  import IR.{ __newVar => _, __assign => _, __ifThenElse => _ , _ }

  private def vals(sym: Sym[Any]) : List[Sym[Any]] = sym match {
    case Def(Reify(s, u, effects)) => if (s.isInstanceOf[Sym[Any]]) List(s.asInstanceOf[Sym[Any]]) else Nil
    case Def(Reflect(NewVar(v), u, effects)) => Nil
    case _ => List(sym)
  }

  private def vars(sym: Sym[Any]) : List[Sym[Any]] = sym match {
    case Def(Reflect(NewVar(v), u, effects)) => List(sym)
    case _ => Nil
  }

  private def mutating(kernelContext: State, sym: Sym[Any]) : List[Sym[Any]] = kernelContext flatMap {
    case Def(Reflect(x,u,effects)) => if (u.mayWrite contains sym) List(sym) else Nil
    case _ => Nil
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = emitAnyNode(List(sym), rhs)
  override def emitFatNode(sym: List[Sym[Any]], rhs: FatDef) = emitAnyNode(sym, rhs)

  var effectWrapper: Option[Sym[Any]] = None 
  private def withEffectWrapper(r: Sym[Any])(block: => Unit) = {
    effectWrapper = Some(r)
    block
    effectWrapper = None
  }
  
  private def emitAnyNode(sym: List[Sym[Any]], rhs: Any): Unit = {
    assert(generators.length >= 1)

    printlog("DeliteGenTaskGraph.emitNode "+sym+"="+rhs)

    val kernelName = sym.map(quote).mkString("")

    var resultIsVar = false
    var skipEmission = false
    var nestedNode: TP[Any] = null
    var external = false

    // we will try to generate any node that is not purely an effect node
    rhs match {
      case Reflect(s, u, effects) =>
        //controlDeps = effects; // <---  now handling control deps here...!! <--- would like to, but need to hand *precise* schedule to runtime
        withEffectWrapper(sym(0)) {
          super.emitNode(sym(0), rhs.asInstanceOf[Def[Any]])
        }
        return
      case Reify(s, u, effects) =>
        //controlDeps = effects
        withEffectWrapper(sym(0)) {
          super.emitNode(sym(0), rhs.asInstanceOf[Def[Any]])
        } 
        return
      case NewVar(x) => resultIsVar = true // if sym is a NewVar, we must mangle the result type
      case e: DeliteOpExternal[_] => external = true
      case _ => // continue and attempt to generate kernel
    }

    // validate that generators agree on inputs (similar to schedule validation in DeliteCodegen)
    //val dataDeps = ifGenAgree(g => (g.syms(rhs) ++ g.getFreeVarNode(rhs)).distinct, true)

    val dataDeps = { // don't use getFreeVarNode...
      val bound = boundSyms(rhs)
      val used = syms(rhs)
      // println( "=== used for " + sym)
      // used foreach { s => s match {
        // case Def(x) => println(s + " = " + x)
        // case _ => println(s)
      // }}
      //println(used)
      //focusFatBlock(used) { freeInScope(bound, used) } filter { case Def(r@Reflect(x,u,es)) => used contains r; case _ => true } // distinct
      focusFatBlock(used.map(Block(_))) { freeInScope(bound, used) } // distinct
      //syms(rhs).flatMap(s => focusBlock(s) { freeInScope(boundSyms(rhs), s) } ).distinct
    }

    val inVals = dataDeps flatMap { vals(_) }
    val inVars = dataDeps flatMap { vars(_) }

    implicit val supportedTargets = new ListBuffer[String]
    implicit val returnTypes = new ListBuffer[Pair[String, String]]
    implicit val outputSlotTypes = new HashMap[String, ListBuffer[(String, String)]]
    implicit val metadata = new ArrayBuffer[Pair[String, String]]

    // parameters for delite overrides
    deliteInputs = (inVals ++ inVars)
    deliteResult = Some(sym) //findDefinition(rhs) map { _.sym }

    /**
     * Run stencil analysis locally on the current op to insure that all transformations, including fusion, have already happened.
     */
    val stencilAnalysis = new StencilAnalysis { val IR: DeliteGenTaskGraph.this.IR.type = DeliteGenTaskGraph.this.IR }
    stencilAnalysis.innerScope = this.innerScope
    
    def gatherStencil(sym: List[Sym[Any]], rhs: Any) = rhs match {
      case SimpleFatLoop(sz,v,body) =>         
        for ((s,d) <- sym.zip(body)) {
          stencilAnalysis.process(s,v,d)
        }
      case SimpleLoop(sz,v,body) =>
        stencilAnalysis.process(sym(0),v,body)        
      case _ =>
    }
     
    gatherStencil(sym,rhs)        
    allStencils = /*allStencils ++*/ stencilAnalysis.getLoopStencils    
    
    /*
    val loopAnalysis = new LoopAnalysis { val IR: DeliteGenTaskGraph.this.IR.type = DeliteGenTaskGraph.this.IR }
    loopAnalysis.innerScope = this.innerScope

    //TODO: get rid of
    rhs match {
      case SimpleFatLoop(_,_,_) | SimpleLoop(_,_,_) =>
        loopAnalysis.analyze(sym, rhs) 
        Predef.println("LOOP ANALYSIS:" + IR.loopMeta)   
        loopAnalysis.mapping(deliteInputs)
      case l:AbstractLoop[_] =>  
        loopAnalysis.analyze(sym, rhs) 
        Predef.println("LOOP ANALYSIS:" + IR.loopMeta)   
        loopAnalysis.mapping(deliteInputs)
      case Reflect(l:AbstractLoop[_],u,es) =>
        loopAnalysis.analyze(sym, rhs) 
        Predef.println("LOOP ANALYSIS:" + IR.loopMeta)   
        loopAnalysis.mapping(deliteInputs)
      case _ => //
    }
    */

    if (!skipEmission) for (gen <- generators) {
      val sep = java.io.File.separator
      val buildPath = Config.buildDir + sep + gen + sep + "kernels" + sep
      val outDir = new File(buildPath); outDir.mkdirs()
      val outFile = new File(buildPath + kernelName + "." + gen.kernelFileExt)
      val kstream = new PrintWriter(outFile)
      val bodyString = new StringWriter()
      val bodyStream = new PrintWriter(bodyString)

      try{
        // DISCUSS: use a predicate instead of inheriting from DeliteOp?
        deliteKernel = rhs match {
//          case op:DeliteFatOp => true
          case op:AbstractFatLoop => true
          case op:AbstractFatIfThenElse => true
          case op:DeliteOp[_] => true
          case _ => false
        }

        def genEmitNode(gen: Generator)(sym: List[Sym[Any]], rhs: Any)(genStream: PrintWriter) = rhs match {
          case fd: FatDef => gen.withStream(genStream)(gen.emitFatNode(sym, fd))
          case d: Def[Any] => {
            assert(sym.length == 1)
            gen.withStream(genStream)(gen.emitNode(sym(0), d))
          }
        }

        //initialize
        gen.kernelInit(sym, inVals, inVars, resultIsVar)

        // emit kernel to bodyStream //TODO: must kernel body be emitted before kernel header?
        genEmitNode(gen)(sym, rhs)(bodyStream)
        bodyStream.flush

        var hasOutputSlotTypes = false

        // TODO: we only want to mangle the result type if it is a delite op AND it will be generated AS a delite
        // op (e.g. not generated by a more specific (DSL) generator).
        // this is an issue, for example, with BLAS and MatrixSigmoid which is only a DeliteOp if BLAS is off.
        // perhaps this should be a (DeliteOp, SingleTask) variant.
        // TR: if we introduce a predicate (e.g. isDeliteOp) instead of always matching on the types this would 
        // be taken care of as well (plus we'd no longer need DeliteIfThenElse, DeliteWhile, ...)
        val resultType: String = (gen.toString, rhs) match {
          case ("scala", op: AbstractFatLoop) =>
            hasOutputSlotTypes = true
            "generated.scala.DeliteOpMultiLoop[" + "activation_"+kernelName + "]"
          case ("scala", op: AbstractFatIfThenElse) =>
            //hasOutputSlotTypes = true
            //"generated.scala.DeliteOpMultiLoop[" + "activation_"+kernelName + "]"
            //TODO: support fat if
            assert(sym.length == 1, "TODO: support fat if")
            gen.remap(sym.head.tp)
          case ("scala", z) => z match {
            case op: AbstractLoop[_] => 
              hasOutputSlotTypes = true
              "generated.scala.DeliteOpMultiLoop[" + "activation_"+kernelName + "]"
            case foreach: DeliteOpForeach2[_,_] => "generated.scala.DeliteOpForeach[" + gen.remap(foreach.v.tp) + "]"
            case foreach: DeliteOpForeachBounded[_,_,_] => "generated.scala.DeliteOpForeach[" + gen.remap(foreach.v.tp) + "]"
            case input: DeliteOpInput[_] =>
              hasOutputSlotTypes = true
              "activation_"+kernelName
            case _ => gen.remap(sym.head.tp)
          }
          case ("cpp", op: AbstractFatLoop) =>
            hasOutputSlotTypes = true
            "DeliteOpMultiLoop_" + kernelName
          case ("cpp", z) => z match {
            case op: AbstractLoop[_] =>
              hasOutputSlotTypes = true
              "DeliteOpMultiLoop_" + kernelName
            case input: DeliteOpInput[_] =>
              hasOutputSlotTypes = true
              "activation_"+kernelName
            case _ => gen.remap(sym.head.tp)
          }
          case ("cuda", op: AbstractFatLoop) =>
            hasOutputSlotTypes = true
            "void"
          case ("cuda", op: AbstractFatIfThenElse) =>
            //hasOutputSlotTypes = true
            assert(sym.length == 1, "TODO: support fat if")
            "void"
          case ("cuda", z) => z match {
            case op: AbstractLoop[_] =>
              hasOutputSlotTypes = true
              "void"
            case _ => "void"
          }
          case ("opencl", op: AbstractFatLoop) =>
            hasOutputSlotTypes = true
            "void"
          case ("opencl", op: AbstractFatIfThenElse) =>
            //hasOutputSlotTypes = true
            assert(sym.length == 1, "TODO: support fat if")
            "void"
          case ("opencl", z) => z match {
            case op: AbstractLoop[_] =>
              hasOutputSlotTypes = true
              "void"
            case _ => "void"
          }
          case _ => 
            assert(sym.length == 1) // if not set hasOutputSlotTypes and use activation record
            gen.remap(sym.head.tp)
        }

        assert(hasOutputSlotTypes || sym.length == 1)

        // emit kernel
        gen.withStream(kstream)(gen.emitFileHeader())
        if (hasOutputSlotTypes) {
          // activation record class declaration
          println("[raghu DeliteGenTaskGraph] gen = " + gen.toString + "hasOutputSlotTypes")
          rhs match {
            case d:Def[Any] =>  gen.withStream(kstream)(gen.emitNodeKernelExtra(sym, d))
            case f:FatDef => gen.withStream(kstream)(gen.emitFatNodeKernelExtra(sym, f))
          }
        }
        else {
          println("[raghu DeliteGenTaskGraph] gen = " + gen.toString + "sym.length = " + sym.length)
        }

        gen.withStream(kstream)(gen.emitKernelHeader(sym, inVals, inVars, resultType, resultIsVar, external))
        kstream.println(bodyString.toString)
        gen.withStream(kstream)(gen.emitKernelFooter(sym, inVals, inVars, resultType, resultIsVar, external))

        // record that this kernel was successfully generated
        supportedTargets += gen.toString
        if (!hasOutputSlotTypes) { // return type is sym type
          if (resultIsVar) {
            gen match {
              case g: ScalaCodegen =>
                returnTypes += new Pair[String,String](g.toString,"generated.scala.Ref[" + g.remap(sym.head.tp) + "]") {
                  override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
                }
              case g: CLikeCodegen =>
                returnTypes += new Pair[String,String](g.toString,g.deviceTarget.toString + "Ref< " + g.remap(sym.head.tp) + g.addRef(g.remap(sym.head.tp)) + " >") {
                  override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
                }
              case _ =>
                returnTypes += new Pair[String,String](gen.toString,gen.remap(sym.head.tp)) {
                  override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
                }     
            }
          } 
          else {
            returnTypes += new Pair[String,String](gen.toString,gen.remap(sym.head.tp)) {
              override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
            }
          }
        } else { // return type is activation record
          returnTypes += new Pair[String,String](gen.toString,"activation_" + kernelName) {
            override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
          }
          for (s <- sym) {
            outputSlotTypes.getOrElseUpdate(quote(s), new ListBuffer) += new Pair[String,String](gen.toString,gen.remap(s.tp)) {
              override def toString = "\"" + _1 + "\" : \"" + _2 + "\""
            }
          }
        }

        //add MetaData
        if (gen.hasMetaData) {
          metadata += new Pair[String,String](gen.toString, gen.getMetaData) {
            override def toString = "\"" + _1 + "\" : " + _2
          }
        }

        kstream.close()
        
      } catch {
        case e:GenerationFailedException => // no generator found
          gen.exceptionHandler(e, outFile, kstream)
          if(Config.dumpException) {
            println(gen.toString + ":" + (sym map(quote)))
            e.printStackTrace
          }
          //if(gen.nested > 1) {
          //  nestedNode = gen.lastNodeAttempted
          //}
        case e:Exception => throw(e)
      }
    }

    if (skipEmission == false && supportedTargets.isEmpty) {
      var msg = "Node " + kernelName + "[" + rhs + "] could not be generated by any code generator"
      //if(nested > 1) msg = "Failure is in nested node " + quote(nestedNode.sym) + "[" + nestedNode.rhs + "]. " + msg
      sys.error(msg)
    }

    val outputs = sym      
    val inputs = deliteInputs

    // visible kernel effects
    val outerKernelEffects = effectWrapper match {
      case Some(Def(Reflect(x,u,es))) => List(effectWrapper.get)
      case Some(Def(Reify(x,u,es))) => es
      case _ => Nil
    }        
    // internal kernel effects (that might write to free variables)
    // ideally these would always be propagated up and internalKernelEffects should not be needed,
    // but i don't think that's always happening now... so this is effectively a safety net
    val defs = rhs match {
      case op:AbstractFatLoop => op.body
      case op:AbstractFatIfThenElse => (op.thenp zip op.elsep) map (p => IfThenElse(op.cond,p._1,p._2))
      case d: Def[Any] => List(d)
    }        
    val internalKernelEffects = getEffectsBlock(defs)
            
    val kernelContext = outerKernelEffects ++ internalKernelEffects
        
    // kernel inputs mutated by any visible effectful operation inside kernel
    val inMutating = (inputs flatMap { mutating(kernelContext, _) }).distinct

    // additional data deps: for each of my inputs, look at the kernels already generated and see if any of them
    // mutate it, and if so, add that kernel as a data-dep
    val extraDataDeps = (kernelMutatingDeps filter { case (s, mutates) => (!(inputs intersect mutates).isEmpty) }).keys
    val inControlDeps = (controlDeps ++ extraDataDeps).distinct

    // anti deps: for each of my mutating inputs, look at the kernels already generated and see if any of them
    // read it, add that kernel as an anti-dep
    val antiDeps = (kernelInputDeps filter { case (s, in) => (!(inMutating intersect in).isEmpty) }).keys.toList

    // add this kernel to global generated state
    sym foreach { s => kernelInputDeps += { s -> inputs } }
    sym foreach { s => kernelMutatingDeps += { s -> inMutating } }

    // debug
    /*
    stream.println("inputs: " + inputs)
    stream.println("mutating inputs: " + inMutating)
    stream.println("extra data deps: " + extraDataDeps)
    stream.println("control deps: " + inControlDeps)
    stream.println("anti deps:" + antiDeps)
    */
    printlog(outputSlotTypes)

    val optContext = sym.find(!_.sourceContexts.isEmpty).map(_.sourceContexts.head)
       
    
    
                
    /**
     * Domain-specific inputs to loops cause issues with the stencil analysis, since it only records accesses on arrays.
     * This method is intended to allow us to associate structs with their component arrays by identifying array symbols
     * that are fields within a struct. However, discovering the original array symbol requires some care since each
     * field read is a new symbol. 
     * 
     * Until we implement this, we can only associate the stencil analysis results with an op's input
     * if input structs are unwrapped.
     */
    /*
    def getArrayInputs(s: Exp[Any]): Seq[Exp[Any]] = s.tp match {
      case StructType(tag,elems) => 
        Predef.println("found struct with elems " + elems)
        elems.flatMap(e => getArrayInputs(e._2))
      case y if y.erasure == manifest[DeliteArray[Any]].erasure => List(s)
      case _ => 
        Predef.println("did not find struct or array: " + findDefinition(s.asInstanceOf[Sym[Any]]).toString)
        Nil
    }
    */
        
    // result is a single stencil representing all the info we have for this op's inputs
    val opStencil = if (sym == Nil) new Stencil() else sym.map(i => allStencils.getOrElse(i, new Stencil())).reduce((a,b) => a ++ b)
    
    // emit task graph node
    rhs match {
      case op: AbstractFatLoop =>
        // Predef.println("emitting DeliteFatLoop (" + sym + "), inputs are: ")
        // Predef.println(inputs)
        // Predef.println("stencil is: " + opStencil)
        // val arrayInputs = inputs.flatMap(getArrayInputs)
        // Predef.println("  array inputs are: ")
        // Predef.println(arrayInputs)
        emitMultiLoop(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, op.size, op.body.exists (loopBodyNeedsCombine _), op.body.exists (loopBodyNeedsPostProcess _), optContext, opStencil)
      case op: AbstractFatIfThenElse =>
        assert(sym.length == 1, "TODO: implement fat if then else")
        emitIfThenElse(Block(op.cond), op.thenp.head, op.elsep.head, kernelName, outputs, resultIsVar, inputs, inMutating, inControlDeps, antiDeps)
      case z =>
        z match {
          case op:AbstractLoop[_] => 
            // Predef.println("emitting DeliteLoop (" + sym + "), inputs are: ")
            // Predef.println(inputs)          
            // Predef.println("stencil is: " + opStencil)
            emitMultiLoop(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, op.size, loopBodyNeedsCombine(op.body), loopBodyNeedsPostProcess(op.body), optContext, opStencil)
          case e:DeliteOpExternal[_] => emitExternal(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps)
          case c:DeliteOpCondition[_] => emitIfThenElse(Block(c.cond), c.thenp, c.elsep, kernelName, outputs, resultIsVar, inputs, inMutating, inControlDeps, antiDeps)
          case w:DeliteOpWhileLoop => emitWhileLoop(w.cond, w.body, kernelName, outputs, resultIsVar, inputs, inMutating, inControlDeps, antiDeps)
          case s:DeliteOpSingleTask[_] => emitSingleTask(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, optContext)
          case i:DeliteOpInput[_] => emitInput(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, optContext)
          case f:DeliteOpForeach2[_,_] => emitForeach(f, kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps)
          case f:DeliteOpForeachBounded[_,_,_] => emitForeach(f, kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps)
          case _ => emitSingleTask(kernelName, outputs, resultIsVar, inputs, inVars, inMutating, inControlDeps, antiDeps, if (outputs(0).sourceContexts.isEmpty) None else Some(outputs(0).sourceContexts.head)) // things that are not specified as DeliteOPs, emit as SingleTask nodes
        }
    }

    // whole program gen (for testing)
    //emitValDef(sym, "embedding.scala.gen.kernel_" + quote(sym) + "(" + inputs.map(quote(_)).mkString(",") + ")")
  }

  
  /**
   * @param sym         the symbol representing the kernel
   * @param inputs      a list of real kernel dependencies (formal kernel parameters)
   * @param controlDeps a list of control dependencies (must execute before this kernel)
   * @param antiDeps    a list of WAR dependencies (need to be committed in program order)
   */

  def emitMultiLoop(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], size: Exp[Int], needsCombine: Boolean, needsPostProcess: Boolean,
                    sourceContext: Option[SourceContext], stencil: Stencil)
       (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
   stream.println("{\"type\":\"MultiLoop\",")
   emitSourceContext(sourceContext, stream, id)
   stream.println(",\n")
   emitConstOrSym(size, "size")
   stream.print(",\"needsCombine\":" + needsCombine)
   stream.println(",\"needsPostProcess\":" + needsPostProcess)   
   emitStencil(inputs, stencil)   
   emitExecutionOpCommon(id, outputs, resultIsVar, inputs, inVars, mutableInputs, controlDeps, antiDeps)
   stream.println("},")      
  }
  
  def emitExternal(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"External\"")
    // TODO: add thread control, other configuration?
    emitExecutionOpCommon(id, outputs, resultIsVar, inputs, inVars, mutableInputs, controlDeps, antiDeps)
    stream.println("},")
  }

  def emitSingleTask(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], sourceContext: Option[SourceContext])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"SingleTask\",")
    emitSourceContext(sourceContext, stream, id)
    emitExecutionOpCommon(id, outputs, resultIsVar, inputs, inVars, mutableInputs, controlDeps, antiDeps)
    stream.println("},")
  }

  def emitInput(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], sourceContext: Option[SourceContext])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Input\",")
    emitSourceContext(sourceContext, stream, id)
    emitExecutionOpCommon(id, outputs, resultIsVar, inputs, inVars, mutableInputs, controlDeps, antiDeps)
    stream.println("},")
  }

  def emitForeach(rhs: Def[Any], id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Foreach\"")
    emitExecutionOpCommon(id, outputs, resultIsVar, inputs, inVars, mutableInputs, controlDeps, antiDeps)
    emitVariant(rhs, id, outputs, resultIsVar, inputs, mutableInputs, controlDeps, antiDeps)
    stream.println("},")
  }

  def emitIfThenElse(cond: Block[Boolean], thenp: Block[Any], elsep: Block[Any], id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.print("{\"type\":\"Conditional\",")
    stream.println("  \"outputId\" : \"" + id + "\",")
    emitSubGraph("cond", cond)
    emitSubGraph("then", thenp)
    emitSubGraph("else", elsep)
    stream.println("  \"condOutput\": \"" + quote(getBlockResult(cond)) + "\",")
    stream.println("  \"thenOutput\": \"" + quote(getBlockResult(thenp)) + "\",")
    stream.println("  \"elseOutput\": \"" + quote(getBlockResult(elsep)) + "\",")
    stream.println("  \"controlDeps\":[" + makeString(controlDeps) + "],")
    stream.println("  \"antiDeps\":[" + makeString(antiDeps) + "],")
    if (remap(thenp.tp) != remap(elsep.tp))
      throw new RuntimeException("Delite conditional with different then and else return types: " + thenp + ", " + remap(thenp.tp) + " and " + elsep + ", " + remap(elsep.tp))
    
    val returnTypesStr = (for (gen <- generators) yield {
      try {
        Some("\"" + gen.toString + "\" : \"" + gen.remap(thenp.tp) + "\"")
      } catch {
        case e:GenerationFailedException => None
        case e:Exception => throw(e)
      }
    }) filter(_.isDefined) map(_.get)   
    
    stream.println("  \"return-types\":{" + returnTypesStr.mkString(",") + "}")
    stream.println("},")
  }

  def emitWhileLoop(cond: Block[Boolean], body: Block[Unit], id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    stream.println("{\"type\":\"WhileLoop\",")
    stream.println("  \"outputId\" : \"" + id + "\",")
    emitSubGraph("cond", cond)
    emitSubGraph("body", body)
    stream.println("  \"condOutput\": \"" + quote(getBlockResult(cond)) + "\",")
    //stream.println("  \"bodyOutput\": \"" + quote(getBlockResult(body)) + "\",")
    stream.println("  \"controlDeps\":[" + makeString(controlDeps) + "],")
    stream.println("  \"antiDeps\":[" + makeString(antiDeps) + "]")
    stream.println("},")
  }

  def emitControlFlowOpCommon(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
  }

  def emitExecutionOpCommon(id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], inVars: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
        (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], outputSlotTypes: HashMap[String, ListBuffer[(String, String)]], metadata: ArrayBuffer[Pair[String,String]]) = {
    
    def wrapRef(gen: GenericCodegen, sym: Exp[Any]):String = {
      if( inVars.contains(sym) || (outputs.contains(sym) && resultIsVar)) {
        gen match {
          case g: ScalaCodegen => "generated.scala.Ref[" + g.remap(sym.tp) + "]"
          case g: CLikeCodegen => g.deviceTarget + "Ref< " + g.remap(sym.tp) + g.addRef(sym.tp) + " >"
          case _ => gen.remap(sym.tp)
        }
      }
      else
        gen.remap(sym.tp)
    }

    // get remap (with wrapping ref) of all targets that can generate the type of the given symbol
    def remapAllTargets(sym: Exp[Any]) = (for (gen <- generators) yield {
      try {
        Some("\"" + gen.toString + "\" : \"" + wrapRef(gen,sym)  + "\"")
      } catch {
        case e:GenerationFailedException => None
        case e:Exception => throw(e)
      }
    }) filter(_.isDefined) map(_.get)  

    stream.print(" , \"kernelId\" : \"" + id + "\" ")
    stream.print(" , \"supportedTargets\": [" + supportedTgt.mkString("\"","\",\"","\"") + "],\n")
    stream.print("  \"outputs\":[" + outputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    stream.print("  \"inputs\":[" + inputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    stream.print("  \"input-types\":{" + inputs.map(s => "\"" + quote(s) + "\":{" + remapAllTargets(s).mkString(",") + "}").mkString(",") + "},\n")
    stream.print("  \"mutableInputs\":[" + mutableInputs.map("\""+quote(_)+"\"").mkString(",") + "],\n")
    emitDepsCommon(controlDeps, antiDeps)
    val metadataStr = if (metadata.isEmpty) "" else metadata.mkString(",")
    stream.print("  \"metadata\":{" + metadataStr + "},\n")
    val returnTypesStr = if(returnTypes.isEmpty) "" else returnTypes.mkString(",")
    if (!outputSlotTypes.isEmpty) {
      stream.println("  \"return-types\":{" + returnTypesStr + "},")
    }
    stream.println("  \"output-types\":{" + outputs.map(s => "\"" + quote(s) + "\":{" + remapAllTargets(s).mkString(",") + "}").mkString(",") +  "}")
  }


  def emitDepsCommon(controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]], last:Boolean = false) {
    stream.print("  \"controlDeps\":[" + makeString(controlDeps) + "],\n")
    stream.print("  \"antiDeps\":[" + makeString(antiDeps) + "]" + (if(last) "\n" else ",\n"))
  }

  def emitStencil(inputs: List[Exp[Any]], stencil: Stencil) = {
    stream.print(" , \"stencil\":{")
    val stencilMap = inputs map { i => 
      "\"" + quote(i) + "\":\"" + (stencil.get(i) match {
        case Some(Interval(mult,stride,len)) => "range(" + quote(mult) + ", " + quote(stride) + ", " + quote(len) + ")"
        case Some(Constant(i)) => "const(" + quote(i) + ")"
        case Some(z) => z.toString
        case None => "none"
      }) + "\""
    }
    stream.print(stencilMap.mkString(","))
    stream.println("}")
  }  
  
  var nested = 0

  def emitVariant(rhs: Def[Any], id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
                 (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) {

    if (!rhs.isInstanceOf[Variant] || nested >= Config.nestedVariantsLevel) return

    nested += 1

    // pre
    val saveMutatingDeps = kernelMutatingDeps
    val saveInputDeps = kernelInputDeps
    kernelMutatingDeps = Map()
    kernelInputDeps = Map()
    stream.print(",\"variant\": {")
    stream.print("\"ops\":[" )

    // variant
    rhs match {
      case mvar:DeliteOpMapLikeWhileLoopVariant => emitMapLikeWhileLoopVariant(mvar, id, outputs, resultIsVar, inputs, mutableInputs, controlDeps, antiDeps)
      case rvar:DeliteOpReduceLikeWhileLoopVariant => emitReduceLikeWhileLoopVariant(rvar, id, outputs, resultIsVar, inputs, mutableInputs, controlDeps, antiDeps)
      case _ =>
    }

    // post
    emitEOG()
    emitOutput(getBlockResult(Block(rhs.asInstanceOf[Variant].variant))) // FIXME: block?
    stream.println("}")
    kernelInputDeps = saveInputDeps
    kernelMutatingDeps = saveMutatingDeps
    
    nested -= 1    
  }

  def emitMapLikeWhileLoopVariant(vw: DeliteOpMapLikeWhileLoopVariant, id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
    (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) {

    // manually lift alloc out of the variant loop. TODO: this should not be required, see comment in DeliteOps.scala
    // we should be able to remove this when we merge with opfusing
    //val save = scope
    emitBlock(Block(vw.alloc)) //FIXME: block?
    //scope = appendScope()
    emitBlock(Block(vw.variant)) //FIXME: block?
    //scope = save
  }

  def emitReduceLikeWhileLoopVariant(vw: DeliteOpReduceLikeWhileLoopVariant, id: String, outputs: List[Exp[Any]], resultIsVar: Boolean, inputs: List[Exp[Any]], mutableInputs: List[Exp[Any]], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]])
    (implicit supportedTgt: ListBuffer[String], returnTypes: ListBuffer[Pair[String, String]], metadata: ArrayBuffer[Pair[String,String]]) {

    //val save = scope
    emitBlock(Block(vw.Index)) //FIXME: block?
    emitBlock(Block(vw.Acc)) //FIXME: block?
    //scope = appendScope()

    def emitSubGraphOp(block: Block[Any], controlDeps: List[Exp[Any]], antiDeps: List[Exp[Any]]) {
      stream.print("{\"type\":\"SubGraph\", ")
      val resultId = if (quote(getBlockResult(block)) == "()") quote(block.res) else quote(getBlockResult(block)) //TODO: :(
      stream.print("\"outputId\":\"" + resultId + "\",\n")
      emitDepsCommon(controlDeps, antiDeps)
      emitSubGraph("", block)
      emitOutput(block.res)
      stream.println("},")
    }

    emitSubGraphOp(Block(vw.init), Nil, Nil) //FIXME: block?
    emitSubGraphOp(Block(vw.variant), List(vw.init), Nil) //FIXME: block?
    //scope = save
  }

  def emitConstOrSym(x: Exp[Any], prefix: String) = x match {
    case c:Const[Any] => stream.println("  \"" + prefix + "Type\": \"const\",")
                         stream.println("  \"" + prefix + "Value\": \"" + quote(x) + "\"")
    case s:Sym[Any] =>   stream.println("  \"" + prefix + "Type\": \"symbol\",")
                         stream.println("  \"" + prefix + "Value\": \"" + quote(getBlockResult(Block(x))) + "\"") // x might be a Reify
  }
  
  def emitOutput(x: Exp[Any]) = emitConstOrSym(x, "output")

  def emitEOG() = {
    stream.print("{\"type\":\"EOG\"}\n],\n")
  }

  def emitSubGraph(prefix: String, e: Block[Any]) = e match {
    case Block(c:Const[Any]) => stream.println("  \"" + prefix + "Type\": \"const\",")
                         stream.println("  \"" + prefix + "Value\": \"" + quote(c) + "\",")
    case Block(s:Sym[Any]) =>  stream.println("  \"" + prefix + "Type\": \"symbol\",")
                        stream.println("  \"" + prefix + "Ops\": [")
                        val saveMutatingDeps = kernelMutatingDeps
                        val saveInputDeps = kernelInputDeps
                        kernelMutatingDeps = Map()
                        kernelInputDeps = Map()
                        emitBlock(e)
                        emitEOG()
                        kernelInputDeps = saveInputDeps
                        kernelMutatingDeps = saveMutatingDeps
  }

  private def makeString(list: List[Exp[Any]]) = {
    if(list.isEmpty) "" else list.map(quote(_)).mkString("\"","\",\"","\"")
  }

/*
  // more quirks
  override def quote(x: Exp[Any]) = x match {
    case r:Reify[Any] => quote(r.x) //DISCUSS <- what's the purpose of this? it will never match because Reify is a Def, not Exp
    case _ => super.quote(x)
  }
*/

  def nop = throw new RuntimeException("Not Implemented Yet")

}
