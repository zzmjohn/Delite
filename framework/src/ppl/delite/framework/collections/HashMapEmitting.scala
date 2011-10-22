package ppl.delite.framework.collections



import java.io.PrintWriter
import ppl.delite.framework.{DeliteApplication, DSLType}
import ppl.delite.framework.datastruct.scala.DeliteCollection
import scala.virtualization.lms.common._
import ppl.delite.framework.ops.DeliteOpsExp



trait HashMapEmitting {
  
  def hashMapEmitterFactory[K, V] = new EmitterFactory {
    def needsCombine = false
    def needsPostProcess = true
    def needsPostProcess2 = true
    
    def scala = new ScalaEmitter {
      def emitBufferDefs(kernelname: String, basename: String, elemtype: String)(implicit stream: PrintWriter) {
        // a temporary hack
        val innertypes = elemtype.substring(13, elemtype.length - 1).split(", ")
        val keytype = innertypes(0)
        val valtype = innertypes(1)
        
        stream.println("// emitting hash map")
        stream.println("var " + basename + "_buf_ind: Array[Int] = _")
        stream.println("var " + basename + "_buf_keys: Array[" + keytype + "] = _")
        stream.println("var " + basename + "_buf_vals: Array[" + valtype + "] = _")
        stream.println("var " + basename + "_buf_spills = new collection.mutable.ArrayBuffer[Int]()")
        stream.println("var " + basename + "_bufsz = 0")
        stream.println("var " + basename + "_chunkIdx: Int = _")
        stream.println("var " + basename + "_numChunks: Int = _")
        stream.println("var " + basename + "_blocks: Int = _")
        stream.println("var " + basename + "_blkbits: Int = _")
        stream.println("var " + basename + "_offset: Int = 0")
        stream.println("var " + basename + "_activations: Array[activation_" + kernelname + "] = _")
        
        stream.println("def " + basename + "_nextPow2(x: Int) = {")
        stream.println("var c = x - 1")
        stream.println("c |= c >>>  1")
        stream.println("c |= c >>>  2")
        stream.println("c |= c >>>  4")
        stream.println("c |= c >>>  8")
        stream.println("c |= c >>> 16")
        stream.println("c + 1")
        stream.println("}")
        
        stream.println("def " + basename + "_grow() {")
        stream.println("if (%s_bufsz > (%s_buf_ind.length * 0.4f)) {".format(basename, basename))
        stream.println("val nindices = Array.fill[Int](%s_buf_ind.length * 2)(-1)".format(basename))
        stream.println("val nkeys = new Array[%s](%s_buf_keys.length * 2)".format(keytype, basename))
        stream.println("val nvals = new Array[%s](%s_buf_keys.length * 2)".format(valtype, basename))
        stream.println("// copy raw data")
        stream.println("System.arraycopy(%s_buf_keys, 0, nkeys, 0, %s_bufsz)".format(basename, basename))
        stream.println("System.arraycopy(%s_buf_vals, 0, nvals, 0, %s_bufsz)".format(basename, basename))
        stream.println("// copy indices")
        stream.println("var i = 0")
        stream.println("val relbits = Integer.numberOfTrailingZeros(nindices.length / 2)")
        stream.println("while (i < %s_buf_ind.length) {".format(basename))
        stream.println("val elem = %s_buf_ind(i)".format(basename))
        stream.println("if (elem != -1) {")
        stream.println("val hash = %s_buf_ind(i + 1)".format(basename))
        stream.println("var pos = (hash >>> (32 - relbits)) * 2")
        stream.println("")
        stream.println("// insert it into nindices")
        stream.println("var currelem = nindices(pos)")
        stream.println("var currhash = nindices(pos + 1)")
        stream.println("while (currelem != -1) {")
        stream.println("pos = (pos + 2) % nindices.length")
        stream.println("currelem = nindices(pos)")
        stream.println("currhash = nindices(pos + 1)")
        stream.println("}")
        stream.println("nindices(pos) = elem")
        stream.println("nindices(pos + 1) = hash")
        stream.println("}")
        stream.println("i += 2")
        stream.println("}")
        stream.println("")
        stream.println("%s_buf_ind = nindices".format(basename))
        stream.println("%s_buf_keys = nkeys".format(basename))
        stream.println("%s_buf_vals = nvals".format(basename))
        stream.println("")
        stream.println("}")
        stream.println("}")
        
        stream.println("def " + basename + "_buf_init(chunkIdx: Int, numChunks: Int, numElems: Int) {")
        stream.println(basename + "_chunkIdx = chunkIdx")
        stream.println(basename + "_numChunks = numChunks")
        stream.println(basename + "_blocks = " + basename + "_nextPow2(numChunks)")
        stream.println(basename + "_blkbits = Integer.numberOfTrailingZeros(" + basename + "_blocks)")
        stream.println("val elemest = math.max(" + basename + "_blocks * 8, numElems / numChunks)")
        stream.println("val tablesize = " + basename + "_nextPow2((elemest / 0.4f).toInt) * 2")
        stream.println(basename + "_buf_ind = Array.fill[Int](tablesize)(-1)")
        stream.println(basename + "_buf_keys = new Array[" + keytype + "]((elemest * 1.2f).toInt)")
        stream.println(basename + "_buf_vals = new Array[" + valtype + "]((elemest * 1.2f).toInt)")
        stream.println("}")
        
        stream.println("def " + basename + "_buf_append(x: " + elemtype + ") {")
        // find relevant hashcode part
        stream.println("val hc = x._1.## * 0x9e3775cd")
        // find starting position
        stream.println("val relbits = Integer.numberOfTrailingZeros(%s_buf_ind.length / 2)".format(basename))
        stream.println("var pos = (hc >>> (32 - relbits)) * 2")
        // find empty slot
        stream.println("var currelem = %s_buf_ind(pos)".format(basename))
        stream.println("var currhash = %s_buf_ind(pos + 1)".format(basename))
        stream.println("while (currelem != -1 && (currhash != hc || %s_buf_keys(currelem) != x._1)) {".format(basename))
        stream.println("pos = (pos + 2) % " + basename + "_buf_ind.length")
        stream.println("currelem = %s_buf_ind(pos)".format(basename))
        stream.println("currhash = %s_buf_ind(pos + 1)".format(basename))
        stream.println("}")
        // add the element
        stream.println("if (currelem == -1) {")
        stream.println("val datapos = %s_bufsz".format(basename))
        stream.println("%s_buf_ind(pos) = datapos".format(basename))
        stream.println("%s_buf_ind(pos + 1) = hc".format(basename))
        stream.println("%s_buf_keys(datapos) = x._1".format(basename))
        stream.println("%s_buf_vals(datapos) = x._2".format(basename))
        stream.println("%s_bufsz += 1".format(basename))
        stream.println("%s_grow()".format(basename))
        stream.println("} else {")
        stream.println("val datapos = currelem")
        stream.println("%s_buf_keys(datapos) = x._1".format(basename))
        stream.println("%s_buf_vals(datapos) = x._2".format(basename))
        stream.println("}")
        stream.println("}")
      }
      def emitInitSubActivation(basename: String, activname: String, chunkIdxVar: String, numChunksVar: String)(implicit stream: PrintWriter) {
        stream.println(activname + "." + basename + "_buf_init(" + chunkIdxVar + ", " + numChunksVar + ", size)")
      }
      def emitAddToBuffer(prefixSym: String, basename: String, elemname: String)(implicit stream: PrintWriter) {
        stream.println(prefixSym + basename + "_buf_append(" + elemname + ")")
      }
      def emitAddToDataStructure(prefixSym: String, basename: String, elemname: String)(implicit stream: PrintWriter) {
        stream.println(prefixSym + basename + ".put(" + elemname + "._1, " + elemname + "._2)")
      }
      def emitPostCombine(basename: String, activname: String, lhsname: String)(implicit stream: PrintWriter) {
        stream.println(activname + "." + basename + "_offset = " + lhsname + "." + basename + "_offset + " + lhsname + "." + basename + "_bufsz")
        
        // init first block
        stream.println("if (%s.%s_chunkIdx == 0) {".format(lhsname, basename))
        stream.println("%s.%s_activations = new Array(%s.%s_numChunks)".format(lhsname, basename, lhsname, basename))
        stream.println("%s.%s_activations(0) = %s".format(lhsname, basename, lhsname))
        stream.println("}")
        
        // copy and update the array
        stream.println("%s.%s_activations = %s.%s_activations".format(activname, basename, lhsname, basename))
        stream.println("%s.%s_activations(%s.%s_chunkIdx) = %s".format(activname, basename, activname, basename, activname))
      }
      def emitPostProcInit(basename: String, activname: String)(implicit stream: PrintWriter) {
        stream.println("if (" + activname + "." + basename + "_offset > 0) {")
        stream.println("val elemest = " + activname + "." + basename + "_offset + " + activname + "." + basename + "_bufsz")
        stream.println("val tablesize = %s.%s_nextPow2((elemest / 0.4f).toInt + 1)".format(activname, basename))
        stream.println("val indextable = Array.fill[Int](tablesize)(-1)")
        stream.println("%s.%s.unsafeSetInternal(indextable, %s.%s.unsafeKeys, %s.%s.unsafeValues, 0)".format(activname, basename, activname, basename, activname, basename))
        stream.println("%s.%s.unsafeSetBlockSizes(new Array[Int](%s.%s_blocks * 32))".format(activname, basename, activname, basename))
        stream.println("} else {")
        stream.println("%s.%s.unsafeSetInternal(%s.%s_buf_ind, %s.%s_buf_keys, %s.%s_buf_vals, %s.%s_bufsz)".format(activname, basename, activname, basename, activname, basename, activname, basename, activname, basename))
        stream.println("%s.%s.unsafeSetBlockSizes(new Array[Int](%s.%s_blocks * 32))".format(activname, basename, activname, basename))
        stream.println("}")
        
        //stream.println("for (act <- %s.%s_activations) println(act.%s_chunkIdx + \"::\", act.%s_buf_ind.mkString(\", \"))".format(activname, basename, basename, basename))
        //stream.println("println(\"----------------------------------------------\")")
      }
      def emitPostProcess(basename: String, activname: String)(implicit stream: PrintWriter) {
        // process all chunks
        stream.println("if (%s.%s_numChunks > 1) {".format(activname, basename))
        stream.println("val mychunk = %s.%s_chunkIdx".format(activname, basename))
        stream.println("var chidx = 0")
        stream.println("val targetindices = %s.%s.unsafeIndices".format(activname, basename))
        stream.println("val targetsizes = %s.%s.unsafeBlockSizes".format(activname, basename))
        stream.println("val relbits = Integer.numberOfTrailingZeros(targetindices.length / 2)".format(basename))
        stream.println("while (chidx < %s.%s_numChunks) {".format(activname, basename))
        stream.println("val act = %s.%s_activations(chidx)".format(activname, basename))
        // process your blocks from the current activation
        stream.println("var blkidx = mychunk")
        stream.println("val indices = act.%s_buf_ind".format(basename))
        stream.println("while (blkidx < %s.%s_blocks) {".format(activname, basename))
        // process the particular block from the current activation
        stream.println("var pos = blkidx * (act.%s_buf_ind.length / act.%s_blocks)".format(basename, basename))
        stream.println("var until = (blkidx + 1) * (act.%s_buf_ind.length / act.%s_blocks)".format(basename, basename))
        stream.println("val tuntil = (blkidx + 1) * (targetindices.length / act.%s_blocks)".format(basename))
        stream.println("var currelem = indices(pos)")
        stream.println("while (pos < until || currelem != -1) {")
        stream.println("val currhash = indices(pos + 1)")
        stream.println("val currblock = currhash >>> (32 - %s.%s_blkbits)".format(activname, basename))
        // skip if the hash indicates the element spilled from the previous block (or belongs to the next - we might have entered the next block in search of spills!)
        stream.println("if (blkidx == currblock) {")
        stream.println("if (currelem != -1) {")
        // find the correct position to put it in
        stream.println("var tpos = (currhash >>> (32 - relbits)) * 2")
        //stream.println("println(Thread.currentThread, %s.%s_chunkIdx, chidx, blkidx, currhash, currelem, tpos)".format(activname, basename))
        stream.println("var placed = false")
        stream.println("while (tpos < tuntil) {")
        stream.println("var telem = targetindices(tpos)")
        stream.println("if (telem == -1) {")
        stream.println("targetindices(tpos) = currelem")
        stream.println("targetindices(tpos + 1) = chidx")
        stream.println("targetsizes(blkidx * 32) += 1")
        stream.println("placed = true")
        stream.println("tpos = tuntil")
        stream.println("} else if (%s.%s_activations(targetindices(tpos + 1)).%s_buf_keys(telem) == act.%s_buf_keys(currelem)) {".format(activname, basename, basename, basename))
        stream.println("targetindices(tpos) = currelem")
        stream.println("targetindices(tpos + 1) = chidx")
        stream.println("placed = true")
        stream.println("tpos = tuntil")
        stream.println("}")
        stream.println("tpos += 2")
        stream.println("}")
        stream.println("if (!placed) act.%s_buf_spills += currelem".format(basename, basename, basename))
        //stream.println("println(currhash, act.%s_buf_spills, targetindices.mkString(\", \"))".format(basename))
        stream.println("}")
        stream.println("}")
        //stream.println("println(pos, until, tuntil, indices.length, currelem)")
        stream.println("pos = (pos + 2) % indices.length")
        stream.println("if (pos == 0) until = 0")
        stream.println("currelem = indices(pos)")
        stream.println("}")
        // move to next block in this chunk
        stream.println("blkidx += %s.%s_numChunks".format(activname, basename))
        stream.println("}")
        // move to next chunk
        stream.println("chidx += 1")
        stream.println("}")
        stream.println("}")
        //stream.println("println(\"chidx(\" + %s.%s_chunkIdx + \")\", %s.%s)".format(activname, basename, activname, basename))
        //stream.println("for (act <- %s.%s_activations) println(act.%s_chunkIdx + \"::\", act.%s_buf_ind.mkString(\", \"))".format(activname, basename, basename, basename))
      }
      def emitPostCombine2(basename: String, activname: String, lhsname: String)(implicit stream: PrintWriter) {
        // count the number of elements in each block
        stream.println("if (%s.%s_numChunks > 1) {".format(activname, basename))
        stream.println("}")
        
        //stream.println("println(%s.%s.unsafeBlockSizes.mkString(\", \"))".format(activname, basename))
        //stream.println("for (act <- %s.%s_activations) println(act.%s_chunkIdx + \"::\" + act.%s_buf_spills)".format(activname, basename, basename, basename))
      }
      def emitPostProcInit2(basename: String, activname: String)(implicit stream: PrintWriter) {
        stream.println("if (%s.%s_numChunks > 1) {".format(activname, basename))
        // 1) put all the spills in proper blocks, and update the count
        stream.println("var chidx = 0")
        stream.println("val targetindices = %s.%s.unsafeIndices".format(activname, basename))
        stream.println("val targetsizes = %s.%s.unsafeBlockSizes".format(activname, basename))
        stream.println("while (chidx < %s.%s_numChunks) {".format(activname, basename))
        stream.println("val curract = %s.%s_activations(chidx)".format(activname, basename))
        stream.println("var relbits = Integer.numberOfTrailingZeros(targetindices.length / 2)")
        //stream.println("println(chidx)")
        stream.println("for (idx <- curract.%s_buf_spills) {".format(basename))
        stream.println("val k = curract.%s_buf_keys(idx)".format(basename))
        stream.println("val v = curract.%s_buf_vals(idx)".format(basename))
        stream.println("val hc = k.## * 0x9e3775cd")
        stream.println("var tpos = (hc >>> (32 - relbits)) * 2")
        stream.println("var telem = targetindices(tpos)")
        stream.println("while (telem != -1 && (%s.%s_activations(targetindices(tpos + 1)).%s_buf_keys(telem) != k)) {".format(activname, basename, basename))
        //stream.println("println(chidx, idx, tpos, telem)")
        stream.println("tpos = (tpos + 2) % targetindices.length")
        stream.println("telem = targetindices(tpos)")
        stream.println("}")
        stream.println("targetindices(tpos) = idx")
        stream.println("targetindices(tpos + 1) = chidx")
        stream.println("if (telem == -1) targetsizes(32 * (hc >>> (32 - %s.%s_blkbits))) += 1".format(activname, basename))
        stream.println("}")
        stream.println("chidx += 1")
        stream.println("}")
        stream.println("")
        
        // 2) count the number of elements in each chunk and set offsets
        //stream.println("println(%s.%s.unsafeBlockSizes.mkString(\", \"))".format(activname, basename))
        stream.println("chidx = 0")
        stream.println("var elemcount = 0")
        stream.println("while (chidx < %s.%s_numChunks) {".format(activname, basename))
        //stream.println("println('counting, chidx, elemcount)")
        stream.println("val curract = %s.%s_activations(chidx)".format(activname, basename))
        stream.println("var blkidx = chidx")
        stream.println("var currchcount = 0")
        stream.println("while (blkidx < %s.%s_blocks) {".format(activname, basename))
        stream.println("currchcount += targetsizes(32 * blkidx)".format(activname, basename))
        //stream.println("println('inblock, chidx, blkidx, targetsizes(32 * blkidx))")
        stream.println("blkidx += %s.%s_numChunks".format(activname, basename))
        stream.println("}")
        stream.println("curract.%s_offset = elemcount".format(basename))
        stream.println("elemcount += currchcount")
        stream.println("chidx += 1")
        //stream.println("println('counting, chidx, elemcount)")
        stream.println("}")
        stream.println("")
        
        // 3) allocate the data table and set size
        //stream.println("println(elemcount)")
        stream.println("%s.%s.unsafeSetKeys(new Array((1.25 * elemcount).toInt))".format(activname, basename))
        stream.println("%s.%s.unsafeSetValues(new Array((1.25 * elemcount).toInt))".format(activname, basename))
        stream.println("%s.%s.unsafeSetSize(elemcount)".format(activname, basename))
        stream.println("%s.%s.unsafeSetBlockSizes(null)".format(activname, basename))
        stream.println("")
        stream.println("}")
        //stream.println("println(\"-------------------------------------------\")")
        //stream.println("println(%s.%s)".format(activname, basename))
      }
      def emitPostProcess2(basename: String, activname: String)(implicit stream: PrintWriter) {
        // update the references to the data table and hashcodes in each block
        stream.println("if (%s.%s_numChunks > 1) {".format(activname, basename))
        // go through your blocks, copy each element into the data table, and store its hashcode
        stream.println("val tindices = %s.%s.unsafeIndices".format(activname, basename))
        stream.println("val tkeys = %s.%s.unsafeKeys".format(activname, basename))
        stream.println("val tvals = %s.%s.unsafeValues".format(activname, basename))
        //stream.println("println(tdata)")
        stream.println("var blkidx = %s.%s_chunkIdx".format(activname, basename))
        stream.println("var datapos = %s.%s_offset".format(activname, basename))
        //stream.println("println(datapos + \", \" + %s.%s_chunkIdx)".format(activname, basename))
        stream.println("while (blkidx < %s.%s_blocks) {".format(activname, basename))
        stream.println("var pos = tindices.length / %s.%s_blocks * blkidx".format(activname, basename))
        stream.println("val until = tindices.length / %s.%s_blocks * (blkidx + 1)".format(activname, basename))
        stream.println("while (pos < until) {")
        stream.println("val currelem = tindices(pos)")
        stream.println("if (currelem != -1) {")
        stream.println("val actidx = tindices(pos + 1)")
        stream.println("val act = %s.%s_activations(actidx)".format(activname, basename))
        stream.println("val k = act.%s_buf_keys(currelem)".format(basename))
        stream.println("val v = act.%s_buf_vals(currelem)".format(basename))
        stream.println("val hc = k.## * 0x9e3775cd")
        stream.println("tindices(pos + 1) = hc")
        stream.println("tindices(pos) = datapos")
        stream.println("tkeys(datapos) = k")
        stream.println("tvals(datapos) = v")
        stream.println("datapos += 1")
        stream.println("}")
        stream.println("pos += 2")
        stream.println("}")
        stream.println("blkidx += %s.%s_numChunks".format(activname, basename))
        stream.println("}")
        stream.println("}")
        //stream.println("println(%s.%s)".format(activname, basename))
      }
    }
  }
  
}


