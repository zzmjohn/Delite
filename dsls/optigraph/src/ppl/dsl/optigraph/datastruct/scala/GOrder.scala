
package ppl.dsl.optigraph.datastruct.scala

import collection.mutable.{HashSet}

class GOrder[@specialized T: ClassManifest] {
  var _dataset: HashSet[T] = new HashSet[T]()
  var _data: Array[T] = new Array[T](0)
  var _start_idx: Int = 0
  var _end_idx: Int = -1
}