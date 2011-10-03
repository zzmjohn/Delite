package ppl.delite.framework.collections.datastruct.scala






final class ArraySeqImpl[@specialized T: Manifest](__length: Int) extends ArraySeq[T] {
  private var _length = __length
  private var _data = new Array[T](__length)
  
  def length = _length
  
  def apply(idx: Int) = _data(idx)
  
  def update(idx: Int, elem: T) = _data(idx) = elem
  
}
