package ppl.dsl.optigraph.datastruct.scala

class GOrder[@specialized T: ClassManifest] {
  var _data: Array[T] = new Array[T](0)
}

