package ppl.dsl.optigraph.ops

import scala.virtualization.lms.common.ScalaOpsPkg
import scala.virtualization.lms.common.{BaseExp, Base, BooleanOps, ExceptionOps}
import ppl.dsl.optigraph.{GIterable, GOrder}
import ppl.dsl.optigraph.{OptiGraphLift, OptiGraphCompiler, OptiGraph}
import ppl.delite.framework.datastructures.DeliteArray

trait GOrderImplOps { this: OptiGraph =>
  //def gorder_dataasarray_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[DeliteArray[A]]
  def gorder_size_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[Int]
  def gorder_apply_impl[A:Manifest](o: Rep[GOrder[A]], i: Rep[Int]): Rep[A]
  //def gorder_update_impl[A:Manifest](o: Rep[GOrder[A]], i: Rep[Int], x: Rep[A]): Rep[Unit]
  def gorder_items_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[GIterable[A]]
  def gorder_contains_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Boolean]
  def gorder_front_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
  def gorder_back_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A]
  def gorder_pushback_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit]
  def gorder_pushbackorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit]
  def gorder_pushfront_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit]
  def gorder_pushfrontorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit]
  def gorder_popfront_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[Unit]
  def gorder_popback_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[Unit]
}

trait GOrderImplOpsStandard extends GOrderImplOps {
  this: OptiGraphCompiler with OptiGraphLift =>

  def gorder_size_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[Int] = {
    gorder_raw_data(o).length
  }

  def gorder_apply_impl[A:Manifest](o: Rep[GOrder[A]], i: Rep[Int]): Rep[A] = {
    val sz = gorder_size(o)
    if (sz == 0)
      fatal("Tried to access element of empty GOrder")
    else if (!(i >= 0 && i < sz)) {
      fatal("GOrder apply: Index " + i + " out of bounds (size=" + sz + ")")
    }
    val d = gorder_raw_data(o)
    d(i)
  }

  def gorder_items_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[GIterable[A]] = {
    if (gorder_size(o) == 0)
      return GIterable[A]()
    val d = gorder_raw_data(o)
    val gi = GIterable[A](d)
    gi
  }

  def gorder_contains_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Boolean] = {
    if (gorder_size(o) == 0)
      return false
    val d = gorder_raw_data(o).map((e) => e == x)
    d.reduce(boolean_or, false)
  }

  def gorder_front_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    if (gorder_size(o) == 0)
      fatal("Tried to access front of empty GOrder")
    gorder_apply(o, 0)
  }

  def gorder_back_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    if (gorder_size(o) == 0)
      fatal("Tried to access back of empty GOrder")
    gorder_apply(o, gorder_size(o) - 1)
  }

  def gorder_pushback_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit] = {
    if (gorder_empty(o) || !gorder_contains(o, x)) {
      val sz = gorder_size(o)
      val r = DeliteArray[A](sz + 1)
      val d = gorder_raw_data(o)
      darray_unsafe_copy(d, 0, r, 0, sz)
      darray_unsafe_update(r, sz, x)
      gorder_set_raw_data(o, r)
    }
  }

  def gorder_pushbackorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit] = {
    for (i <- 0 until gorder_size(xs)) {
      gorder_pushback(o, gorder_apply(xs, i))
    }
  }

  def gorder_pushfront_impl[A:Manifest](o: Rep[GOrder[A]], x: Rep[A]): Rep[Unit] = {
    if(gorder_empty(o) || !gorder_contains(o, x)) {
      val sz = gorder_size(o)
      val r = DeliteArray[A](sz + 1)
      val d = gorder_raw_data(o)
      darray_unsafe_copy(d, 0, r, 1, sz) //TODO check format of this function
      darray_unsafe_update(r, 0, x)
      gorder_set_raw_data(o, r)
    }
  }

  def gorder_pushfrontorder_impl[A:Manifest](o: Rep[GOrder[A]], xs: Rep[GOrder[A]]): Rep[Unit] = {
    var i = gorder_size(xs) - 1
    while (i >= 0) {
      gorder_pushfront(o, gorder_apply(xs, i))
      i -= 1
    }
  }

  def gorder_popfront_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    val f = gorder_front(o)
    val sz = gorder_size(o)
    val r = DeliteArray[A](sz - 1)
    val d = gorder_raw_data(o)
    darray_unsafe_copy(d, 1, r, 0, sz - 1)
    gorder_set_raw_data(o, r)
    f
  }

  def gorder_popback_impl[A:Manifest](o: Rep[GOrder[A]]): Rep[A] = {
    val l = gorder_back(o)
    val sz = gorder_size(o)
    val r = DeliteArray[A](sz - 1)
    val d = gorder_raw_data(o)
    darray_unsafe_copy(d, 0, r, 0, sz - 1)
    gorder_set_raw_data(o, r)
    l
  }

  private def gorder_empty[A:Manifest](o: Rep[GOrder[A]]): Rep[Boolean] = {
    gorder_size(o) == 0
  }
}