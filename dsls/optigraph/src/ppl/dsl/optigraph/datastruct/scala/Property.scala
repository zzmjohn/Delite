package ppl.dsl.optigraph.datastruct.scala

/**
 * A Property object is used to associate data with graph nodes or edges
 * When the Property object is first created, the data value of each node/edge is set
 * to the default of the data type (e.g. Boolean -> False, Int -> 0, String -> null)
 * Note: properties can be associated only with immutable graph instances
 */

class Property[@specialized T: ClassManifest](val g: Graph, val size: Int) {
  /* Stores the property value for each graph node/edge */
  var data: Array[T] = new Array[T](size)
  /* Stores the deferred property value for each graph node/edge */
  var deferred_data: Array[T] = new Array[T](size)
  /* Indicates if there is a deferred value for a given node/edge */
  var deferred_data_bitmap: Array[Boolean] = new Array[Boolean](size)
}