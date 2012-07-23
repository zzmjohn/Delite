package ppl.dsl.optiml.matrix

import java.io.{PrintWriter}
import scala.reflect.SourceContext
import scala.virtualization.lms.common.{DSLOpsExp,FunctionBlocksExp}
import scala.virtualization.lms.common.{VariablesExp, Variables}
import scala.virtualization.lms.common.{CudaGenBase, ScalaGenBase, CGenBase, CGenFat, OpenCLGenBase}
import scala.virtualization.lms.internal.{GenerationFailedException}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.datastruct.scala.DeliteCollection
import ppl.delite.framework.ops.DeliteOpsExp
import ppl.delite.framework.Config
import ppl.delite.framework.extern.lib._
import ppl.dsl.optila.SparseTransform
import ppl.dsl.optiml._

trait OptiMLDenseMatrixOps extends ppl.dsl.optila.matrix.DenseMatrixOps {
  this: OptiML =>
  
  implicit def denseToMatOverrides[A:Manifest](x: Rep[DenseMatrix[A]]) = new OptiMLDenseMatOpsOverrides(x)  
}

trait OptiMLSparseMatrixOps extends ppl.dsl.optila.matrix.SparseMatrixOps {
  this: OptiML =>
  
  implicit def sparseToMatOverrides[A:Manifest](x: Rep[SparseMatrix[A]]) = new OptiMLSparseMatOpsOverrides(x)  
}

trait ImageOpsExtension extends ImageOps {
  this: OptiML =>
  
  implicit def imageToMatOverrides[A:Manifest](x: Rep[Image[A]]) = new OptiMLImageOpsOverrides(x)  
}

trait MatrixOps extends ppl.dsl.optila.matrix.MatrixOps  {
  this: OptiML =>

  trait OptiMLMatOpsOverrides[A] extends MatOpsCls[A] {
    def apply(rowIndices: Interface[IndexVector])(implicit ctx: SourceContext) = matrix_apply_row_indices[A,IA,MA](x, rowIndices)
    def apply(rowIndices: Interface[IndexVector], colIndices: IndexWildcard)(implicit ctx: SourceContext) = matrix_apply_row_indices[A,IA,MA](x, rowIndices)
    def apply(rowIndices: IndexWildcard, colIndices: Interface[IndexVector])(implicit ctx: SourceContext) = matrix_apply_col_indices[A,IA,MA](x, colIndices)
    def apply(rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit ctx: SourceContext) = matrix_apply_block_indices[A,IA,MA](x, rowIndices, colIndices)    
  }
  
  class OptiMLDenseMatOpsOverrides[A:Manifest](x: Rep[DenseMatrix[A]]) extends DenseMatOpsCls(x) with OptiMLMatOpsOverrides[A] 
  class OptiMLSparseMatOpsOverrides[A:Manifest](x: Rep[SparseMatrix[A]]) extends SparseMatOpsCls(x) with OptiMLMatOpsOverrides[A] {
    // the SourceContext implicit is ambiguous with the possible following apply (Vector constructor) call
    //def nzRowIndices(implicit ctx: SourceContext) = sparsematrix_nz_row_indices(x)
    def nzRowIndices = sparsematrix_nz_row_indices(x)
  }
  class OptiMLImageOpsOverrides[A:Manifest](x: Rep[Image[A]]) extends ImageOpsCls(x) with OptiMLMatOpsOverrides[A] 

  // class defs
  def matrix_apply_row_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext): Rep[MA] 
  def matrix_apply_col_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext): Rep[MA]   
  def matrix_apply_block_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext): Rep[MA]  
  
  def sparsematrix_nz_row_indices[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext): Rep[IndexVectorDense]
}

trait MatrixOpsExp extends ppl.dsl.optila.matrix.MatrixOpsExp with MatrixOps with VariablesExp with SparseTransform {
  this: OptiMLExp  =>
  
  // for c generation
  case class MatrixMultiplyVectorizedFromIndexVectors[A:Manifest:Arith](lrows: Rep[IndexVectorRange], lcols: Rep[IndexVectorRange], lblk: Block2[Int,Int,A],
                                                                        rrows: Rep[IndexVectorRange], rcols: Rep[IndexVectorRange], rblk: Block2[Int,Int,A])
    extends Def[DenseMatrix[A]] {

    val mA = manifest[A]  
    val a = implicitly[Arith[A]]
  }
  
  case class MatrixMultiplyVectorizedFromViews[A:Manifest](lnr: Rep[Int], lnc: Rep[Int], laxis: Axis, ln: Sym[Int], lm: Sym[Int], lview: Block[A], rnr: Rep[Int], rnc: Rep[Int], raxis: Axis, rn: Sym[Int], rm: Sym[Int], rview: Block[A])
    extends Def[DenseMatrix[A]] {

    val mA = manifest[A]  
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case e@MatrixMultiplyVectorizedFromViews(lnr, lnc, laxis, ln, lm, lview, rnr, rnc, raxis, rn, rm, rview) => syms(lview) ::: syms(lnr) ::: syms(lnc) ::: syms(rview) ::: syms(rnr) ::: syms(rnc)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e@MatrixMultiplyVectorizedFromViews(lnr, lnc, laxis, ln, lm, lview, rnr, rnc, raxis, rn, rm, rview) => scala.List(ln,lm,rn,rm) ::: effectSyms(lview) ::: effectSyms(rview)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e@MatrixMultiplyVectorizedFromViews(lnr, lnc, laxis, ln, lm, lview, rnr, rnc, raxis, rn, rm, rview) => freqNormal(lview) ::: freqNormal(lnr) ::: freqNormal(lnc) ::: freqNormal(rview) ::: freqNormal(rnr) ::: freqNormal(rnc)
    case _ => super.symsFreq(e)
  } 
  
  ////////////////////////////////
  // implemented via delite ops

  case class MatrixApplyRowIndices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit val b: MatrixBuilder[A,I,MA])
    extends DeliteOpSingleWithManifest2[A,I,MA](reifyEffectsHere(matrix_apply_row_indices_impl[A,I,MA](x,rowIndices)))       
  
  case class MatrixApplyColIndices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit val b: MatrixBuilder[A,I,MA])
    extends DeliteOpSingleWithManifest2[A,I,MA](reifyEffectsHere(matrix_apply_col_indices_impl[A,I,MA](x,colIndices)))       

  case class MatrixApplyBlockIndices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit val b: MatrixBuilder[A,I,MA])
    extends DeliteOpSingleWithManifest2[A,I,MA](reifyEffectsHere(matrix_apply_block_indices_impl[A,I,MA](x,rowIndices,colIndices)))       
    
  case class SparseMatrixNZRowIndices[A:Manifest](x: Rep[SparseMatrix[A]]) extends DeliteOpSingleWithManifest[A,IndexVectorDense](reifyEffectsHere(sparsematrix_csr_nz_row_indices_impl(x)))
  
  /////////////////////
  // class interface

  def matrix_apply_row_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext)
    = reflectPure(MatrixApplyRowIndices[A,I,MA](x,rowIndices))
  def matrix_apply_col_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext)
    = reflectPure(MatrixApplyColIndices[A,I,MA](x,colIndices))
  def matrix_apply_block_indices[A:Manifest,I:Manifest,MA:Manifest](x: Interface[Matrix[A]], rowIndices: Interface[IndexVector], colIndices: Interface[IndexVector])(implicit b: MatrixBuilder[A,I,MA], ctx: SourceContext)
    = reflectPure(MatrixApplyBlockIndices[A,I,MA](x,rowIndices,colIndices))

  def sparsematrix_nz_row_indices[A:Manifest](x: Rep[SparseMatrix[A]])(implicit ctx: SourceContext) = reflectPure(SparseMatrixNZRowIndices(x))
  
  def sparsematrix_maprowstovecnz[A:Manifest,B:Manifest](x: Exp[SparseMatrix[A]], f: Exp[Int] => Exp[B], isRow: Exp[Boolean]) = {
    val indices = x.nzRowIndices
    val outIndices = densevector_raw_data(indices).take(indices.length) // trim
    val outData = outIndices.map(f)     
    val out = SparseVector[B](x.numRows, isRow)
    sparsevector_set_raw_indices(out, outIndices.unsafeImmutable)
    sparsevector_set_raw_data(out, outData)
    sparsevector_set_nnz(out, outData.length)
    out.unsafeImmutable          
  }
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@MatrixApplyRowIndices(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixApplyRowIndices(f(x),f(y))(e.mA,e.mB,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])  
    case e@MatrixApplyColIndices(x,y) => reflectPure(new { override val original = Some(f,e) } with MatrixApplyColIndices(f(x),f(y))(e.mA,e.mB,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])      
    case e@MatrixApplyBlockIndices(x,r,c) => reflectPure(new { override val original = Some(f,e) } with MatrixApplyBlockIndices(f(x),f(r),f(c))(e.mA,e.mB,e.mR,e.b))(mtype(manifest[A]),implicitly[SourceContext])        
    case e@SparseMatrixNZRowIndices(x) => reflectPure(new { override val original = Some(f,e) } with SparseMatrixNZRowIndices(f(x))(e.mA))(mtype(manifest[A]),implicitly[SourceContext])        
    case Reflect(e@MatrixApplyRowIndices(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixApplyRowIndices(f(x),f(y))(e.mA,e.mB,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@MatrixApplyColIndices(x,y), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixApplyColIndices(f(x),f(y))(e.mA,e.mB,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))    
    case Reflect(e@MatrixApplyBlockIndices(x,r,c), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with MatrixApplyBlockIndices(f(x),f(r),f(c))(e.mA,e.mB,e.mR,e.b), mapOver(f,u), f(es)))(mtype(manifest[A]))        
    case Reflect(e@SparseMatrixNZRowIndices(x), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with SparseMatrixNZRowIndices(f(x))(e.mA), mapOver(f,u), f(es)))(mtype(manifest[A]))        
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??

  //////////////
  // transforms
  
  def specializeSparseMapRows[A:Manifest,B:Manifest](s: Sym[Any], e: MatrixMapRowsToVec[A,B,_], x: Exp[SparseMatrix[A]], rowFunc: Interface[Vector[A]] => Exp[B], isRow: Exp[Boolean]): Option[Exp[Any]] = {
    val default = ZeroVector[A](x.numRows, true) 
    val test = reifyEffects(rowFunc(default))
    val repr = e.body.asInstanceOf[DeliteCollectElem[B,_,_]].func
    if (test.res == defaultValue[B]) {  
      val t = deviceIndependentLowering
      Some(s.atPhase(t) {                        
        // TODO: repr still slices the input x by non-zero row (still operates on the sparse structure)
        // any way to operate only on the dense csr arrays? (pretty tricky, since rowFunc is defined on Interface[Vector[A]]))
        sparsematrix_maprowstovecnz(t(x), { a => transformBlockWithBound(t, repr, scala.List(e.fin -> a)) }, t(isRow))
      })
    }        
    else {
      warn("performance: mapRows function " + repr + " operates on zero values of sparse object " + findDefinition(x.asInstanceOf[Sym[Any]]).get.toString) // TODO: context for v
      None
    }          
  }
  
  // hacks for manifest
  // def isSparseMatAny[A:Manifest](x: Exp[Any]) = isSparseMat(x.asInstanceOf[Exp[DeliteCollection[A]]])
  // def asSparseMatAny[A:Manifest](x: Exp[Any]) = asSparseMat(x.asInstanceOf[Exp[DeliteCollection[A]]])
  
  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = d match {    
    case e@MatrixMapRowsToVec(x,rf,isrow) if (Config.optimize > 0 && isSparseMat(x.ops.elem.asInstanceOf[Exp[DeliteCollection[Any]]])) =>
      specializeSparseMapRows(s, e, asSparseMat(x.ops.elem.asInstanceOf[Exp[DeliteCollection[Any]]]), rf, isrow)(e.mA,e.mB).map(_.asInstanceOf[Exp[A]]) getOrElse super.onCreate(s,d)
    case Reflect(e@MatrixMapRowsToVec(x,rf,isrow), u, es) if (Config.optimize > 0 && isSparseMat(x.ops.elem.asInstanceOf[Exp[DeliteCollection[Any]]])) =>
      reflectSpecialized(specializeSparseMapRows(s, e, asSparseMat(x.ops.elem.asInstanceOf[Exp[DeliteCollection[Any]]]), rf, isrow)(e.mA,e.mB), u, es)(super.onCreate(s,d))    
      
    case _ => super.onCreate(s,d)
  }
  
  
}

/**
 *  Optimizations for composite MatrixOps operations.
 :q*/

trait MatrixOpsExpOpt extends ppl.dsl.optila.matrix.MatrixOpsExpOpt with MatrixOpsExp {
  this: OptiMLExp =>
  
  // override def matrix_numrows[A:Manifest](x: Exp[Matrix[A]])(implicit ctx: SourceContext) = x match {
  //   //case Def(TrainingSetObjectFromMat(x,y)) => matrix_numrows(x) // TODO: move to TrainingSetOpsExpOpt ?
  //   case _ => super.matrix_numrows(x)
  // }
  // 
  // override def matrix_numcols[A:Manifest](x: Exp[Matrix[A]])(implicit ctx: SourceContext) = x match {
  //   //case Def(TrainingSetObjectFromMat(x,y)) => matrix_numcols(x) // TODO: move to TrainingSetOpsExpOpt ?
  //   case _ => super.matrix_numcols(x)
  // }
}

trait DenseMatrixOpsExpOpt extends ppl.dsl.optila.matrix.DenseMatrixOpsExpOpt {
  this: OptiMLExp =>    

  override def densematrix_multiply[A:Manifest:Arith](x: Rep[DenseMatrix[A]], y: Rep[DenseMatrix[A]])(implicit ctx: SourceContext) = (x,y) match {
    case (Def(DenseMatrixViewNew(l,lr,lc,laxis,ln,lm,lv)), Def(DenseMatrixViewNew(r,rr,rc,raxis,rn,rm,rv))) if (Config.generateCpp) =>
      reflectPure(MatrixMultiplyVectorizedFromViews(lr,lc,laxis,ln,lm,lv,rr,rc,raxis,rn,rm,rv))
    
      case (Def(IndexVector2Construct(lrows, lcols, f, fblk)), Def(IndexVector2Construct(rrows, rcols, g, gblk))) if (Config.generateCpp && Config.optimize > 0) =>
      //Predef.println("found matrix constructor multiply")
      // TODO: really need to check and handle ranges and dense indexvector separately
      reflectPure(MatrixMultiplyVectorizedFromIndexVectors(lrows.ops.elem.asInstanceOf[Rep[IndexVectorRange]],lcols.ops.elem.asInstanceOf[Rep[IndexVectorRange]],fblk,rrows.ops.elem.asInstanceOf[Rep[IndexVectorRange]],rcols.ops.elem.asInstanceOf[Rep[IndexVectorRange]],gblk))
      //super.densematrix_multiply(x,y)
    case _ => super.densematrix_multiply(x,y)
  } 

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case e@MatrixMultiplyVectorizedFromIndexVectors(lrows,lcols,fblk,rrows,rcols,gblk) => reflectPure(MatrixMultiplyVectorizedFromIndexVectors(f(lrows),f(lcols),Block2(f(fblk.blockArg1).asInstanceOf[Sym[Int]],f(fblk.blockArg2).asInstanceOf[Sym[Int]],f(fblk.blockRes))(manifest[Int],manifest[Int],e.mA),f(rrows),f(rcols),Block2(f(gblk.blockArg1).asInstanceOf[Sym[Int]],f(gblk.blockArg2).asInstanceOf[Sym[Int]],f(gblk.blockRes))(manifest[Int],manifest[Int],e.mA))(e.mA,e.a))(mtype(manifest[A]), implicitly[SourceContext])

    case e@MatrixMultiplyVectorizedFromViews(lr,lc,laxis,ln,lm,lv,rr,rc,raxis,rn,rm,rv) => reflectPure(MatrixMultiplyVectorizedFromViews(f(lr),f(lc),laxis,f(ln).asInstanceOf[Sym[Int]],f(lm).asInstanceOf[Sym[Int]],f(lv),f(rr),f(rc),raxis,f(rn).asInstanceOf[Sym[Int]],f(rm).asInstanceOf[Sym[Int]],f(rv))(e.mA))(mtype(manifest[A]), implicitly[SourceContext])

    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}


trait ScalaGenMatrixOps extends ScalaGenBase {
  val IR: MatrixOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MatrixMultiplyVectorizedFromIndexVectors(lrows,lcols,lblk,rrows,rcols,rblk) => stream.println("val " + quote(sym) + ": " + remap(sym.tp) + " = throw new UnsupportedOperationException(\"should never be executed\")")
    case MatrixMultiplyVectorizedFromViews(lnr,lnc,laxis,ln,lm,lv,rnr,rnc,raxis,rn,rm,rv) => stream.println("val " + quote(sym) + ": " + remap(sym.tp) + " = throw new UnsupportedOperationException(\"should never be executed\")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenMatrixOps extends CudaGenBase with CudaGenDataStruct {
  val IR: MatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait OpenCLGenMatrixOps extends OpenCLGenBase with OpenCLGenDataStruct {
  val IR: MatrixOpsExp
  import IR._

  // override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
  //   case _ => super.emitNode(sym, rhs)
  // }
}

trait CGenMatrixOps extends CGenFat {
  val IR: MatrixOpsExp with FunctionBlocksExp
  import IR._

  // TODO: factor out common parts
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
     case e@MatrixMultiplyVectorizedFromViews(lnr,lnc,laxis,ln,lm,lv,rnr,rnc,raxis,rn,rm,rv) => 
        //stream.println("val " + quote(sym) + ": " + remap(sym.tp) + " = throw new UnsupportedOperationException(\"should never be executed\")")
        stream.println("// Will have 3 garbage elements at the end")
        stream.println("float M_"+quote(sym)+"[12] __attribute__ ((aligned (16)));")
        stream.println("{")
        stream.println("int nrealatoms = "+quote(lnc)+";")
        stream.println("int npaddedatoms = nrealatoms;")
        stream.println("while (npaddedatoms % 4 != 0) npaddedatoms += 1;")
        stream.println("int niters = npaddedatoms >> 2;")                                                          
        stream.println("__m128 xx,xy,xz,yx,yy,yz,zx,zy,zz;")
        stream.println("__m128 ax,ay,az,b;")
        stream.println("__m128 t0,t1,t2;")
        stream.println("// Prologue")
        stream.println("xx = _mm_xor_ps(xx,xx);")                                                                                                          
        stream.println("xy = _mm_xor_ps(xy,xy);") 
        stream.println("xz = _mm_xor_ps(xz,xz);")                                                                                                     
        stream.println("yx = _mm_xor_ps(yx,yx);") 
        stream.println("yy = _mm_xor_ps(yy,yy);") 
        stream.println("yz = _mm_xor_ps(yz,yz);") 
        stream.println("zx = _mm_xor_ps(zx,zx);") 
        stream.println("zy = _mm_xor_ps(zy,zy);") 
        stream.println("zz = _mm_xor_ps(zz,zz);")          
        stream.println("float *aTx, *aTy, *aTz, *bTx, *bTy, *bTz;")
        stream.println("int " + quote(lm) + " = 0;")
        stream.println("int " + quote(rm) + " = 0;")
        stream.println("int " + quote(ln) + " = 0;")
        stream.println("int " + quote(rn) + " = 0;")
        
        // views l & r must be row-padded ahead of time!
        emitBlock(lv)
        stream.println("aTx = &"+quote(getBlockResult(lv))+"[0];")
        stream.println("aTy = &"+quote(getBlockResult(lv))+"[npaddedatoms];")
        stream.println("aTz = &"+quote(getBlockResult(lv))+"[2*npaddedatoms];")
        emitBlock(rv)
        stream.println("bTx = &"+quote(getBlockResult(rv))+"[0];")
        stream.println("bTy = &"+quote(getBlockResult(rv))+"[npaddedatoms];")
        stream.println("bTz = &"+quote(getBlockResult(rv))+"[2*npaddedatoms];") 

        stream.println("for (int k = 0; k < niters; k++) {")        
        stream.println("")
        stream.println("    ax = _mm_load_ps(aTx);")                                                                                                        
        stream.println("    ay = _mm_load_ps(aTy);") 
        stream.println("    az = _mm_load_ps(aTz);")                                                         
        stream.println("")
        stream.println("    b = _mm_load_ps(bTx);") 
        stream.println("    t0 = ax;") 
        stream.println("    t1 = ay;")                                                                                                      
        stream.println("    t2 = az;") 
        stream.println("    t0 = _mm_mul_ps(t0,b);") 
        stream.println("    t1 = _mm_mul_ps(t1,b);") 
        stream.println("    t2 = _mm_mul_ps(t2,b);") 
        stream.println("")
        stream.println("    xx = _mm_add_ps(xx,t0);") 
        stream.println("    yx = _mm_add_ps(yx,t1);") 
        stream.println("    zx = _mm_add_ps(zx,t2);") 
        stream.println("")
        stream.println("    b = _mm_load_ps(bTy);") 
        stream.println("    t0 = ax;") 
        stream.println("    t1 = ay;") 
        stream.println("    t2 = az;") 
        stream.println("")
        stream.println("    t0 = _mm_mul_ps(t0,b);") 
        stream.println("    t1 = _mm_mul_ps(t1,b);") 
        stream.println("    t2 = _mm_mul_ps(t2,b);") 
        stream.println("")
        stream.println("    xy = _mm_add_ps(xy,t0);") 
        stream.println("    yy = _mm_add_ps(yy,t1);") 
        stream.println("    zy = _mm_add_ps(zy,t2);") 
        stream.println("")
        stream.println("    b = _mm_load_ps(bTz);") 
        stream.println("")
        stream.println("    ax = _mm_mul_ps(ax,b);") 
        stream.println("    ay = _mm_mul_ps(ay,b);") 
        stream.println("    az = _mm_mul_ps(az,b);") 
        stream.println("")
        stream.println("    xz = _mm_add_ps(xz,ax);") 
        stream.println("    yz = _mm_add_ps(yz,ay);") 
        stream.println("    zz = _mm_add_ps(zz,az);") 
        stream.println("")
        stream.println("    aTx += 4;") 
        stream.println("    aTy += 4;") 
        stream.println("    aTz += 4;") 
        stream.println("    bTx += 4;") 
        stream.println("    bTy += 4;") 
        stream.println("    bTz += 4;") 
        stream.println("}") 
        stream.println("// Epilogue - reduce 4 wide vectors to one wide") 
        stream.println("xx = _mm_hadd_ps(xx,xy);") 
        stream.println("xz = _mm_hadd_ps(xz,yx);") 
        stream.println("yy = _mm_hadd_ps(yy,yz);") 
        stream.println("zx = _mm_hadd_ps(zx,zy);") 
        stream.println("zz = _mm_hadd_ps(zz,zy);") 

        stream.println("xx = _mm_hadd_ps(xx,xz);") 
        stream.println("yy = _mm_hadd_ps(yy,zx);") 
        stream.println("zz = _mm_hadd_ps(zz,xz);") 

        stream.println("_mm_store_ps(M_"+quote(sym)+"  , xx);") 
        stream.println("_mm_store_ps(M_"+quote(sym)+"+4, yy);") 
        stream.println("_mm_store_ps(M_"+quote(sym)+"+8, zz);") 
        stream.println("}") 
        emitValDef(sym, "new DenseMatrix<"+remap(e.mA)+">(M_"+quote(sym)+", 3, 3)") 
     
     
     case e@MatrixMultiplyVectorizedFromIndexVectors(lrows,lcols,lblk,rrows,rcols,rblk) => 
        //stream.println("// vectorized mat mult here!")
        // TODO: currently assuming 3 x N * N x 3 (from MSMBuilder) 
        val ai = quote(lblk.blockArg1)
        val aj = quote(lblk.blockArg2)
        val bi = quote(rblk.blockArg1)
        val bj = quote(rblk.blockArg2)
        stream.println("// Will have 3 garbage elements at the end")
        stream.println("float M_"+quote(sym)+"[12] __attribute__ ((aligned (16)));")
        stream.println("{")
        stream.println("int nrealatoms = "+quote(lcols)+"->end - "+quote(lcols)+"->start;")
        stream.println("int npaddedatoms = nrealatoms;")
        stream.println("while (npaddedatoms % 4 != 0) npaddedatoms += 1;")
        stream.println("int niters = npaddedatoms >> 2;")                                                          
        stream.println("__m128 xx,xy,xz,yx,yy,yz,zx,zy,zz;")
        stream.println("__m128 ax,ay,az,b;")
        stream.println("__m128 t0,t1,t2;")
        stream.println("// Prologue")
        stream.println("xx = _mm_xor_ps(xx,xx);")                                                                                                          
        stream.println("xy = _mm_xor_ps(xy,xy);") 
        stream.println("xz = _mm_xor_ps(xz,xz);")                                                                                                     
        stream.println("yx = _mm_xor_ps(yx,yx);") 
        stream.println("yy = _mm_xor_ps(yy,yy);") 
        stream.println("yz = _mm_xor_ps(yz,yz);") 
        stream.println("zx = _mm_xor_ps(zx,zx);") 
        stream.println("zy = _mm_xor_ps(zy,zy);") 
        stream.println("zz = _mm_xor_ps(zz,zz);")          
        stream.println("int " + ai + " = 0;")
        stream.println("int " + aj + " = 0;")
        stream.println("int " + bi + " = 0;")
        stream.println("int " + bj + " = 0;")
        stream.println("float aTx[4], aTy[4], aTz[4];")
        stream.println("float bTx[4], bTy[4], bTz[4];")
        stream.println("for (int k = 0; k < niters; k++) {")                                                  
        stream.println("int ka = k*4;")        
        stream.println("int end_real = 4;")
        stream.println("if (ka+4 > nrealatoms) end_real = nrealatoms+4 - npaddedatoms;")
        stream.println("for (int i = 0; i < end_real; i++) {")
        stream.println(aj + " = ka+i;")
        stream.println(bi + " = ka+i;")

        stream.println(ai + " = 0; // x")
        stream.println(bj + " = 0; // x")
        stream.println("{")
        emitBlock(lblk.blockRes)
        stream.println("aTx[i] = " + quote(getBlockResult(lblk.blockRes)) + ";")
        stream.println("}")
        stream.println("{")
        emitBlock(rblk.blockRes)
        stream.println("bTx[i] = " + quote(getBlockResult(rblk.blockRes)) + ";")
        stream.println("}")

        stream.println(ai + " = 1; // y")
        stream.println(bj + " = 1; // y")
        stream.println("{")
        emitBlock(lblk.blockRes)
        stream.println("aTy[i] = " + quote(getBlockResult(lblk.blockRes)) + ";")
        stream.println("}")
        stream.println("{")
        emitBlock(rblk.blockRes)
        stream.println("bTy[i] = " + quote(getBlockResult(rblk.blockRes)) + ";")
        stream.println("}")

        stream.println(ai + " = 2; // z")
        stream.println(bj + " = 2; // z")
        stream.println("{")
        emitBlock(lblk.blockRes)
        stream.println("aTz[i] = " + quote(getBlockResult(lblk.blockRes)) + ";")
        stream.println("}")
        stream.println("{")
        emitBlock(rblk.blockRes)
        stream.println("bTz[i] = " + quote(getBlockResult(rblk.blockRes)) + ";")
        stream.println("}")
        stream.println("}")

        stream.println("for (int i = end_real; i < 4; i++) {")
        stream.println("  aTx[i] = 0; aTy[i] = 0; aTz[i] = 0;")
        stream.println("  bTx[i] = 0; bTy[i] = 0; bTz[i] = 0;")
        stream.println("}")

        stream.println("")
        stream.println("    ax = _mm_load_ps(aTx);")                                                                                                        
        stream.println("    ay = _mm_load_ps(aTy);") 
        stream.println("    az = _mm_load_ps(aTz);")                                                         
        stream.println("")
        stream.println("    b = _mm_load_ps(bTx);") 
        stream.println("    t0 = ax;") 
        stream.println("    t1 = ay;")                                                                                                      
        stream.println("    t2 = az;") 
        stream.println("    t0 = _mm_mul_ps(t0,b);") 
        stream.println("    t1 = _mm_mul_ps(t1,b);") 
        stream.println("    t2 = _mm_mul_ps(t2,b);") 
        stream.println("")
        stream.println("    xx = _mm_add_ps(xx,t0);") 
        stream.println("    yx = _mm_add_ps(yx,t1);") 
        stream.println("    zx = _mm_add_ps(zx,t2);") 
        stream.println("")
        stream.println("    b = _mm_load_ps(bTy);") 
        stream.println("    t0 = ax;") 
        stream.println("    t1 = ay;") 
        stream.println("    t2 = az;") 
        stream.println("")
        stream.println("    t0 = _mm_mul_ps(t0,b);") 
        stream.println("    t1 = _mm_mul_ps(t1,b);") 
        stream.println("    t2 = _mm_mul_ps(t2,b);") 
        stream.println("")
        stream.println("    xy = _mm_add_ps(xy,t0);") 
        stream.println("    yy = _mm_add_ps(yy,t1);") 
        stream.println("    zy = _mm_add_ps(zy,t2);") 
        stream.println("")
        stream.println("    b = _mm_load_ps(bTz);") 
        stream.println("")
        stream.println("    ax = _mm_mul_ps(ax,b);") 
        stream.println("    ay = _mm_mul_ps(ay,b);") 
        stream.println("    az = _mm_mul_ps(az,b);") 
        stream.println("")
        stream.println("    xz = _mm_add_ps(xz,ax);") 
        stream.println("    yz = _mm_add_ps(yz,ay);") 
        stream.println("    zz = _mm_add_ps(zz,az);") 
        stream.println("")
//        stream.println("    aTx += 4;") 
//        stream.println("    aTy += 4;") 
//        stream.println("    aTz += 4;") 
//        stream.println("    bTx += 4;") 
//        stream.println("    bTy += 4;") 
//        stream.println("    bTz += 4;") 
        stream.println("}") 
        stream.println("// Epilogue - reduce 4 wide vectors to one wide") 
        stream.println("xx = _mm_hadd_ps(xx,xy);") 
        stream.println("xz = _mm_hadd_ps(xz,yx);") 
        stream.println("yy = _mm_hadd_ps(yy,yz);") 
        stream.println("zx = _mm_hadd_ps(zx,zy);") 
        stream.println("zz = _mm_hadd_ps(zz,zy);") 

        stream.println("xx = _mm_hadd_ps(xx,xz);") 
        stream.println("yy = _mm_hadd_ps(yy,zx);") 
        stream.println("zz = _mm_hadd_ps(zz,xz);") 

        stream.println("_mm_store_ps(M_"+quote(sym)+"  , xx);") 
        stream.println("_mm_store_ps(M_"+quote(sym)+"+4, yy);") 
        stream.println("_mm_store_ps(M_"+quote(sym)+"+8, zz);") 
        stream.println("}") 
        emitValDef(sym, "new DenseMatrix<"+remap(e.mA)+">(M_"+quote(sym)+", 3, 3)")
     case _ => super.emitNode(sym, rhs)
   }
}
