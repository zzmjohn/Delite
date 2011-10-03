package ppl.delite.framework.collections.datastruct.scala



import ppl.delite.framework.datastruct.scala.DeliteCollection



trait Traversable[@specialized T] extends DeliteCollection[T] {
  def size: Int
  
  def dcSize = size
}


trait Seq[@specialized T] extends Traversable[T] {
  def length: Int
  def apply(idx: Int): T
  def update(idx: Int, x: T)
  
  def size = length
  def dcApply(idx: Int) = apply(idx)
  def dcUpdate(idx: Int, x: T) = update(idx, x)
}


trait ArraySeq[@specialized T] extends Seq[T] {
}
