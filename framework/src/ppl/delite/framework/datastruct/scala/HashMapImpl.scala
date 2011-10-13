package ppl.delite.framework.datastruct.scala






final class HashMapImpl[@specialized K: Manifest, @specialized V: Manifest](_indices: Array[Int], _data: Array[AnyRef], _sz: Int) extends HashMap[K, V] {
  private val loadfactor_d2 = 0.4f / 2
  private var indices = _indices
  private var data = _data
  private var sz = _sz
  
  def size = sz
  
  def get(k: K): V = {
    val hc = k.##
    var pos = (hc % (indices.length / 2)) * 2
    var currelem = indices(pos)
    var currhash = indices(pos + 1)
    
    while (currelem != -1 && (currhash != hc || data(currelem).asInstanceOf[V] != k)) {
      pos = (pos + 2) % indices.length
      currelem = indices(pos)
      currhash = indices(pos + 1)
    }
    
    if (currelem == -1) null.asInstanceOf[V]
    else data(currelem + 1).asInstanceOf[V]
  }
  
  def put(k: K, v: V) {
    val hc = k.##
    var pos = (hc % (indices.length / 2)) * 2
    var currelem = indices(pos)
    var currhash = indices(pos + 1)
    
    while (currelem != -1 && (currhash != hc || data(currelem).asInstanceOf[K] != k)) {
      pos = (pos + 2) % indices.length
      currelem = indices(pos)
      currhash = indices(pos + 1)
    }
    
    if (currelem == -1) {
      val datapos = sz * 2
      indices(pos) = datapos
      data(datapos) = k.asInstanceOf[AnyRef]
      data(datapos + 1) = v.asInstanceOf[AnyRef]
      sz += 1
      
      grow()
    } else {
      val datapos = currelem
      data(datapos) = k.asInstanceOf[AnyRef]
      data(datapos + 1) = v.asInstanceOf[AnyRef]
    }
  }
  
  private def grow() = if (sz > (loadfactor_d2 * indices.length)) {
    val nindices = new Array[Int](indices.length * 2)
    val ndata = new Array[AnyRef](data.length * 2)
    
    // copy raw data
    System.arraycopy(data, 0, ndata, 0, sz * 2)
    
    // copy indices
    var i = 0
    val totalnindices = nindices.length / 2
    while (i < indices.length) {
      val elem = indices(i)
      if (elem != -1) {
        val hash = indices(i + 1)
        var pos = (hash % totalnindices) * 2
        
        // insert it into nindices
        var currelem = nindices(pos)
        var currhash = nindices(pos + 1)
        while (currelem != -1) {
          pos = (pos + 2) % nindices.length
          currelem = nindices(pos)
          currhash = nindices(pos + 1)
        }
        nindices(pos) = elem
        nindices(pos + 1) = hash
      }
      i += 2
    }
    
    indices = nindices
    data = ndata
  }
  
  def dcApply(idx: Int) = {
    val pos = idx * 2
    (data(pos).asInstanceOf[K], data(pos + 1).asInstanceOf[V])
  }
  
  def dcUpdate(idx: Int, x: (K, V)) = throw new UnsupportedOperationException
  
  override def toString = "HashMapImpl(sz: %d; indices: %s; data: %s)".format(sz, indices.mkString(", "), data.mkString(", "))
  
}



