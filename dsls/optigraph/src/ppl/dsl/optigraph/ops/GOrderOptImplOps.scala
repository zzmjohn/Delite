package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, BooleanOps, ExceptionOps}
import scala.collection.mutable.Set
import ppl.dsl.optigraph.{GIterable, GOrder}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}
import ppl.delite.framework.datastructures.DeliteArray

/*
trait GOrderImplOps { this: OptiGraph =>
  def gorder_size_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[Int]
  def gorder_items_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[GIterable[A]]
  def gorder_contains_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Boolean]
  def gorder_front_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
  def gorder_back_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
  def gorder_pushback_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit]
  def gorder_pushbackorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit]
  def gorder_pushfront_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit]
  def gorder_pushfrontorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit]
  def gorder_popfront_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
  def gorder_popback_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
  def gorder_apply_impl[A:Manifest](o: Rep[GOrder[A]], i: Rep[Int]): Rep[A]
}
*/
trait GOrderOptImplOpsStandard extends GOrderImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def gorder_size_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[Int] = {
    gorder_raw_dataset(o.unsafeImmutable).size
  }

  def gorder_items_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[GIterable[A]] = {
    if (gorder_empty(o))
    {
      GIterable[A]()
    }
    else
    {
      val sz = gorder_size(o)
      val d = gorder_raw_data(o)
      val r = DeliteArray[A](sz)
      var i = 0
      while(i < sz)
      {
        val x = gorder_apply(o, i)
        darray_unsafe_update(r, i, x)
        i += 1
      }
      val gi = GIterable[A](r)
      gi
    }
  }

  def gorder_contains_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Boolean] = {
    gorder_raw_dataset(o.unsafeImmutable).contains(x)
  }

// get the i'th element where i = 0,1,..,num of elements in order - 1; offset (i.e. + start_idx) done automatically
  def gorder_apply_impl[A:Manifest](o: Rep[GOrder[A]], i: Rep[Int]): Rep[A] = {
    val sz = gorder_size(o)
    if (gorder_empty(o))
      fatal("Tried to access element of empty GOrder")
    else if (!(i >= 0 && i < sz)) {
      fatal("GOrder apply: Index " + i + " out of bounds (size=" + sz + ")")
    }
    val d = gorder_raw_data(o)
    val arr_sz = d.length
    val start_idx = gorder_start_idx(o)
    val idx = (start_idx + i) % arr_sz
    d.apply(idx)
  }

  def gorder_front_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    if (gorder_empty(o))
      fatal("Tried to access front of empty GOrder")
    gorder_apply(o, 0)
  }

  def gorder_back_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    if (gorder_empty(o))
      fatal("Tried to access back of empty GOrder")
    gorder_apply(o, gorder_size(o)-1)
  }

  def gorder_pushback_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit] = {
    if(!gorder_contains(o, x)){
      val sz = gorder_size(o)
      val d = gorder_raw_data(o)
      val arr_sz = d.length
      // double the space if the array is too full
      if(sz+1 > arr_sz)
      {
        val r = DeliteArray[A](2*arr_sz + 1)
        if(sz > 0)
        {
          val start_idx = gorder_start_idx(o)
          val end_idx = gorder_end_idx(o)
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
        gorder_set_raw_data(o, r)
        gorder_set_start_idx(o, 0)
        gorder_set_end_idx(o, sz)
      }
      // update "in place"
      else
      {
        var end_idx = gorder_end_idx(o)
        end_idx = (end_idx + 1) % arr_sz
        darray_unsafe_update(d, end_idx, x)
        gorder_set_raw_data(o, d)
        gorder_set_end_idx(o, end_idx)
      }
      gorder_raw_dataset(o.unsafeImmutable).add(x.unsafeImmutable)
    }
  }

  def gorder_pushbackorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit] = {
    val xs_size = gorder_size(xs)
    var i = 0
    while(i < xs_size)
    {
      gorder_pushback(o, gorder_apply(xs, i))
      i += 1
    }
  }

  def gorder_pushfront_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit] = {
    if(!gorder_contains(o, x))
    {
      val sz = gorder_size(o)
      val d = gorder_raw_data(o)
      val arr_sz = d.length
      // double the space if the array is too full
      if(sz+1 > arr_sz)
      {
        val r = DeliteArray[A](2*arr_sz + 1)
        if(sz > 0)
        {
          val start_idx = gorder_start_idx(o)
          val end_idx = gorder_end_idx(o)
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
        gorder_set_raw_data(o, r)
        gorder_set_start_idx(o, 0)
        gorder_set_end_idx(o, sz)
      }
      // update "in place"
      else
      {
        var start_idx = gorder_start_idx(o) - 1
        if(start_idx < 0)
        {
          start_idx = start_idx + arr_sz
        }
        darray_unsafe_update(d, start_idx, x)
        gorder_set_raw_data(o, d)
        gorder_set_start_idx(o, start_idx)
      }
      gorder_raw_dataset(o.unsafeImmutable).add(x.unsafeImmutable)
    }
  }

  def gorder_pushfrontorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit] = {
    val xs_size = gorder_size(xs)
    var i = xs_size-1
    while(i >= 0)
    {
      gorder_pushfront(o, gorder_apply(xs, i))
      i -= 1
    }
  }

  def gorder_popfront_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    if(gorder_empty(o))
      fatal("Trying to pop from an empty GOrder")
    val x = gorder_front(o)
    val sz = gorder_size(o)
    val d = gorder_raw_data(o)
    val arr_sz = d.length
    // half the space if the array is too empty
    if(sz-1 <= arr_sz/2)
    {
      val r = DeliteArray[A](arr_sz/2)
      if(sz > 0)
      {
        val start_idx = gorder_start_idx(o)
        val end_idx = gorder_end_idx(o)
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
      gorder_set_raw_data(o, r)
      gorder_set_start_idx(o, 0)
      gorder_set_end_idx(o, sz-2)
    }
    // update "in place"
    else
    {
      var start_idx = gorder_start_idx(o)
      start_idx = (start_idx + 1) % arr_sz
      gorder_set_start_idx(o, start_idx)
    }
    gorder_raw_dataset(o.unsafeImmutable).remove(x.unsafeImmutable)
    x
  }

  def gorder_popback_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    if(gorder_empty(o))
      fatal("Trying to pop from an empty GOrder")
    val x = gorder_back(o)
    val sz = gorder_size(o)
    val d = gorder_raw_data(o)
    val arr_sz = d.length
    // half the space if the array is too empty
    if(sz-1 <= arr_sz/2)
    {
      val r = DeliteArray[A](arr_sz/2)
      if(sz > 0)
      {
        var start_idx = gorder_start_idx(o)
        var end_idx = gorder_end_idx(o)
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
      gorder_set_raw_data(o, r)
      gorder_set_start_idx(o, 0)
      gorder_set_end_idx(o, sz-2)
    }
    // update "in place"
    else
    {
      var end_idx = gorder_end_idx(o) - 1
      if(end_idx < 0)
      {
        end_idx = end_idx + arr_sz
      }
      gorder_set_end_idx(o, end_idx)
    }
    gorder_raw_dataset(o.unsafeImmutable).remove(x.unsafeImmutable)  
    x
  }

  protected def gorder_empty[A:Manifest](o: Rep[GOrder[A]]): Rep[Boolean] = {
    gorder_size(o) == 0
  }
}