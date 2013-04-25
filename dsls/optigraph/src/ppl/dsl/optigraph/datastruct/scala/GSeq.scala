package ppl.dsl.optigraph.datastruct.scala

class GSeq[@specialized T: ClassManifest] {
  var _start_idx: Int = 0
  var _end_idx: Int = -1
  var _data: Array[T] = new Array[T](0)
}