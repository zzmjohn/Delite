package ppl.delite.framework.datastruct.scala






final class ArrayBufferImpl[@specialized T: Manifest](__length: Int) extends ArrayBuffer[T] {
  private var _length = __length
  private var _data = new Array[T](__length)
  
  def length = _length
  
  def apply(idx: Int) = _data(idx)
  
  def update(idx: Int, elem: T) = _data(idx) = elem
  
  def +=(elem: T) {
    if (_length >= _data.length) grow()
    _data(_length) = elem
    _length += 1
  }
  
  private def grow() {
    val narr = new Array[T](_data.length * 2)
    System.arraycopy(_data, 0, narr, 0, _length)
    _data = narr
  }
  
  def unsafeData: Array[T] = _data
  
  def unsafeLength: Int = _length
  
  def unsafeSetData(dat: Array[T], len: Int) {
    _data = dat
    _length = len
  }
  
  override def toString = "ArrayBufferImpl(sz: %d; %s)".format(_length, _data.mkString(", "))
  
}
