package ppl.delite.framework.datastruct.scala






final class ArrayBufferImpl[@specialized T: Manifest](__length: Int) extends ArrayBuffer[T] {
  private var _length = __length
  private var _data = new Array[T](__length)
  
  def length = _length
  
  def apply(idx: Int) = _data(idx)
  
  def update(idx: Int, elem: T) = _data(idx) = elem
  
  def unsafeData: Array[T] = _data
  
  def unsafeLength: Int = _length
  
  def unsafeSetData(dat: Array[T], len: Int) {
    _data = dat
    _length = len
  }
  
  override def toString = "ArrayBufferImpl(sz: %d; %s)".format(_length, _data.mkString(", "))
  
}
