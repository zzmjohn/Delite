package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, BooleanOps, ExceptionOps}
import ppl.dsl.optigraph.{GIterable, GSeq}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}
import ppl.delite.framework.datastructures.DeliteArray

trait GSeqImplOps { this: OptiGraph =>
  def gseq_size_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[Int]
  def gseq_apply_impl[A:Manifest](o: Rep[GSeq[A]], i: Rep[Int]): Rep[A]
  def gseq_items_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[GIterable[A]]
  def gseq_contains_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Boolean]
  def gseq_front_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A]
  def gseq_back_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A]
  def gseq_pushback_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Unit]
  def gseq_pushbackseq_impl[A:Manifest](o: Rep[GSeq[A]], xs: Rep[GSeq[A]]): Rep[Unit]
  def gseq_pushfront_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Unit]
  def gseq_pushfrontseq_impl[A:Manifest](o: Rep[GSeq[A]], xs: Rep[GSeq[A]]): Rep[Unit]
  def gseq_popfront_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A]
  def gseq_popback_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A]
}

trait GSeqImplOpsStandard extends GSeqImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  /* number of elements in the GSeq */
  def gseq_size_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[Int] = {
    val start_idx = gseq_start_idx(o)
    var end_idx = gseq_end_idx(o)
    if(start_idx > end_idx)
    {
      val arr_sz = gseq_raw_data(o).length
      end_idx += arr_sz
    }
    end_idx - start_idx + 1
  }

// get the i'th element where i = 0,1,..,num of elements in seq - 1; offset (i.e. + start_idx) done automatically
  def gseq_apply_impl[A:Manifest](o: Rep[GSeq[A]], i: Rep[Int]): Rep[A] = {
    val sz = gseq_size(o)
    if (gseq_empty(o))
      fatal("Tried to access element of empty GSeq")
    else if (!(i >= 0 && i < sz)) {
      fatal("GSeq apply: Index " + i + " out of bounds (size=" + sz + ")")
    }
    val d = gseq_raw_data(o)
    val arr_sz = d.length
    val start_idx = gseq_start_idx(o)
    val idx = (start_idx + i) % arr_sz
    d.apply(idx)
  }

  def gseq_items_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[GIterable[A]] = {
    if (gseq_empty(o))
      return GIterable[A]()
    val sz = gseq_size(o)
    val d = gseq_raw_data(o)
    val r = DeliteArray[A](sz)
    var i = 0
    while(i < sz)
    {
      val x = gseq_apply(o, i)
      darray_unsafe_update(r, i, x)
      i += 1
    }
    val gi = GIterable[A](r)
    gi
  }

  def gseq_contains_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Boolean] = {
    var i = 0
    val sz = gseq_size(o)
    val d = gseq_raw_data(o)
    var conts = false
    while (i < sz && !conts)
    {
      if (d.apply(i) == x)
      {
        conts = true
      }
      else
      {
        conts = false
      }
      i = i+1
    }
    conts
  }


  def gseq_front_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A] = {
    if (gseq_empty(o))
      fatal("Tried to access front of empty GSeq")
    gseq_apply(o, 0)
  }

  def gseq_back_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A] = {
    if (gseq_empty(o))
      fatal("Tried to access back of empty GSeq")
    gseq_apply(o, gseq_size(o) - 1)
  }

  def gseq_pushback_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Unit] = {
    val sz = gseq_size(o)
    val d = gseq_raw_data(o)
    val arr_sz = d.length
    // double the space if the array is too full
    if(sz+1 > arr_sz)
    {
      val r = DeliteArray[A](2*arr_sz + 1)
      if(sz > 0)
      {
        val start_idx = gseq_start_idx(o)
        val end_idx = gseq_end_idx(o)
        if(start_idx <= end_idx)
        {
          darray_unsafe_copy(d, start_idx, r, 0, sz)
        }
        else
        {
          darray_unsafe_copy(d, start_idx, r, 0, arr_sz-start_idx)
          darray_unsafe_copy(d, 0, r, arr_sz-start_idx, end_idx+1)
        }
      }
      darray_unsafe_update(r, sz, x)
      gseq_set_raw_data(o, r)
      gseq_set_start_idx(o, 0)
      gseq_set_end_idx(o, sz)
    }
    // update "in place"
    else
    {
      var end_idx = gseq_end_idx(o)
      end_idx = (end_idx + 1) % arr_sz
      darray_unsafe_update(d, end_idx, x)
      gseq_set_raw_data(o, d)
      gseq_set_end_idx(o, end_idx)
    }
  }

  def gseq_pushbackseq_impl[A:Manifest](o: Rep[GSeq[A]], xs: Rep[GSeq[A]]): Rep[Unit] = {
    val sz = gseq_size(xs)
    var i = 0
    while(i < sz){
      gseq_pushback(o, gseq_apply(xs, i))
      i+=1
    }
  }

  def gseq_pushfront_impl[A:Manifest](o: Rep[GSeq[A]], x: Rep[A]): Rep[Unit] = {
    val sz = gseq_size(o)
    val d = gseq_raw_data(o)
    val arr_sz = d.length
    // double the space if the array is too full
    if(sz+1 > arr_sz)
    {
      val r = DeliteArray[A](2*arr_sz + 1)
      if(sz > 0)
      {
        val start_idx = gseq_start_idx(o)
        val end_idx = gseq_end_idx(o)
        if(start_idx <= end_idx)
        {
          darray_unsafe_copy(d, start_idx, r, 1, sz)
        }
        else
        {
          darray_unsafe_copy(d, start_idx, r, 1, arr_sz-start_idx)
          darray_unsafe_copy(d, 0, r, arr_sz-start_idx+1, end_idx+1)
        }
      }
      darray_unsafe_update(r, 0, x)
      gseq_set_raw_data(o, r)
      gseq_set_start_idx(o, 0)
      gseq_set_end_idx(o, sz)
    }
    // update "in place"
    else
    {
      var start_idx = gseq_start_idx(o) - 1
      // bug: for some reason this test is ignored (assumed always false) -  see x118.scala in generated scala code
      if(start_idx < 0)
      {
        start_idx = start_idx + arr_sz
      }
      darray_unsafe_update(d, start_idx, x)
      gseq_set_raw_data(o, d)
      gseq_set_start_idx(o, start_idx)
    }
  }

  def gseq_pushfrontseq_impl[A:Manifest](o: Rep[GSeq[A]], xs: Rep[GSeq[A]]): Rep[Unit] = {
    var i = gseq_size(xs) - 1
    while (i >= 0) {
      gseq_pushfront(o, gseq_apply(xs, i))
      i -= 1
    }
  }

  def gseq_popfront_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A] = {
    val f = gseq_front(o)
    val sz = gseq_size(o)
    val d = gseq_raw_data(o)
    val arr_sz = d.length
    // half the space if the array is too empty
    if(sz-1 <= arr_sz/2)
    {
      val r = DeliteArray[A](arr_sz/2)
      if(sz > 0)
      {
        val start_idx = gseq_start_idx(o)
        val end_idx = gseq_end_idx(o)
        if(start_idx <= end_idx)
        {
          darray_unsafe_copy(d, start_idx+1, r, 0, sz-1)
        }
        else
        {
          darray_unsafe_copy(d, start_idx+1, r, 0, arr_sz-start_idx-1)
          darray_unsafe_copy(d, 0, r, arr_sz-start_idx-1, end_idx+1)
        }
      }
      gseq_set_raw_data(o, r)
      gseq_set_start_idx(o, 0)
      gseq_set_end_idx(o, sz-2)
    }
    // update "in place"
    else
    {
      var start_idx = gseq_start_idx(o)
      start_idx = (start_idx + 1) % arr_sz
      gseq_set_start_idx(o, start_idx)
    }
    f
  }

  def gseq_popback_impl[A:Manifest](o: Rep[GSeq[A]]): Rep[A] = {
    val l = gseq_back(o)
    val sz = gseq_size(o)
    val d = gseq_raw_data(o)
    val arr_sz = d.length
    // half the space if the array is too empty
    if(sz-1 <= arr_sz/2)
    {
      val r = DeliteArray[A](arr_sz/2)
      if(sz > 0)
      {
        var start_idx = gseq_start_idx(o)
        var end_idx = gseq_end_idx(o)
        if(start_idx <= end_idx)
        {
          darray_unsafe_copy(d, start_idx, r, 0, sz-1)
        }
        else
        {
          darray_unsafe_copy(d, start_idx, r, 0, arr_sz-start_idx)
          darray_unsafe_copy(d, 0, r, arr_sz-start_idx, end_idx)
        }
      }
      gseq_set_raw_data(o, r)
      gseq_set_start_idx(o, 0)
      gseq_set_end_idx(o, sz-2)
    }
    // update "in place"
    else
    {
      var end_idx = gseq_end_idx(o) - 1
      if(end_idx < 0)
      {
        end_idx = end_idx + arr_sz
      }
      gseq_set_end_idx(o, end_idx)
    }
    l
  }

  protected def gseq_empty[A:Manifest](o: Rep[GSeq[A]]): Rep[Boolean] = {
    gseq_size(o) == 0
  }
}