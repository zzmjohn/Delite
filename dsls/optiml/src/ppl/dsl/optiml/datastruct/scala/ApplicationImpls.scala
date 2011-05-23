package ppl.dsl.optiml.datastruct.scala

class DenoiseVertexDataImpl(val _id : Int, var _belief : Vector[Double], var _potential : Vector[Double]) extends DenoiseVertexData {
  def id = _id

  def belief = _belief
  def setBelief(b: Vector[Double]) = {
    _belief = b
  }
  def belief_=(b: Vector[Double]) = {
    _belief = b
  }

  def potential = _potential
}

class DenoiseEdgeDataImpl(var _msg : Vector[Double], var _oldMsg : Vector[Double]) extends DenoiseEdgeData {
  def message = _msg
  def setMessage(msg: Vector[Double]) = {
    _msg = msg
  }
  def message_=(msg: Vector[Double]) = {
    _msg = msg
  }

  def oldMessage = _oldMsg
  def setOldMessage(msg: Vector[Double]) = {
    _oldMsg = msg
  }
  def oldMessage_=(msg: Vector[Double]) = {
    _oldMsg = msg
  }

  def cloneL = {
    new DenoiseEdgeDataImpl(_msg.cloneL, _oldMsg.cloneL)
  }
}

class BiGGDetection(
  val name: String,
  val score: Float,
  val roi: Rect,
  val mask: GrayscaleImage,
  val index: Int,
  val x: Int,
  val y: Int,
  val tpl: BinarizedGradientTemplate,
  val crt_tpl: BinarizedGradientTemplate
)

class BinarizedGradientPyramid(
  val pyramid: Vector[GrayscaleImage],
  val start_level: Int,
  val levels: Int,
  val fixedLevelIndex: Int
)

class BinarizedGradientTemplate (
  // In the reduced image. The side of the template square is then 2*r+1.
  val radius: Int,

  // Holds a tighter bounding box of the object in the original image scale
  val rect: Rect,
  val mask_list: Vector[Int],

  // Pyramid level of the template (reduction_factor = 2^level)
  val level: Int,

  // The list of gradients in the template
  val binary_gradients: Vector[Int],

  // indices to use for matching (skips zeros inside binary_gradients)
  val match_list: IndexVector,

  // This is a match list of list of sub-parts. Currently unused.
  val occlusions: Vector[Vector[Int]],

  val templates: Vector[BinarizedGradientTemplate],

  val hist: Vector[Float]
)

class Rect(
  val x: Int,
  val y: Int,
  val width: Int,
  val height: Int
)

case class APair (_1: ACluster, _2: Double) extends Ordered[APair] {
  def compare(that: APair) = if(this._2 < that._2) -1 else if(this._2 > that._2) 1 else 0
}

class PQ(val fold:Int) {

  var Q = new java.util.PriorityQueue[APair]()
  var MMD: Double = scala.Double.MaxValue

  def empty = Q.isEmpty

  def normalize(){
    var flag = true
    while(!Q.isEmpty && flag){
      if(Q.element._2 >= MMD)
        Q.remove()
      else
        flag = false
    }
  }

  def push(c:ACluster, d:Double){
    if(c.merged)
      MMD = if (MMD>d) d else MMD
    else if (d < MMD){
      Q.add(APair(c,d))
      if (Q.size > fold)
        Q.remove()
    }
  }

  def push(p:APair){
    val c = p._1
    val d = p._2
    if(c.merged)
      MMD = if (MMD>d) d else MMD
    else if (d < MMD){
      Q.add(APair(c,d))
      if (Q.size > fold)
        Q.remove()
    }
  }

  def top = Q.element._1

  def pop(){
    Q.remove()
  }
}

class ACluster(val dim: Int) extends Ordered[ACluster] {

  var centers: Vector[Double] = null
  var index: Int = -1
  var offset: Int = -1
  var valid: Boolean = false
  var merged: Boolean = false
  var members: Vector[Int] = null
  var num_members: Int = 0
  var data: Vector[Double] = null

  def compare(that:ACluster) = this.num_members - that.num_members

  def absdist(_data1:Vector[Double], offset1:Int, _data2:Vector[Double], offset2:Int, length:Int) : Double = {
    var sum:Double = 0
    var idx = 0
    while(idx < length){
      val tmp = _data1(offset1+idx) - _data2(offset2+idx)
      sum += java.lang.Math.abs(tmp)
      idx += 1
    }
    sum
  }

  def init_RM(d:Vector[Double], c:Vector[Double], m:Vector[Int], i:Int){
    data = d
    centers = c
    index = i
    offset = i*dim
    valid = true
    merged = false
    members = m
    members(0) = i*dim
    num_members = 1
  }
  
  def reset_RM(){
    merged = false
    var i = 0
    while(i < dim){
      ColLT_RM.set(i)
      nth_element(members, 0, num_members/2, num_members)
      centers(offset+i) = data(members(num_members/2)+i)
      i += 1
    }
  }

  def getCandidates(from: ACluster) = {
    var d = scala.Double.MaxValue
    if(index != from.index){
      var i = 0
      while(i < from.num_members){
        val dist = absdist(centers, offset, from.data, from.members(i), dim)
        if (d > dist) d = dist
        i += 1
      }
    }
    Pair(from, d)
  }

  def mergeCandidates(candidates:Vector[APair], fold:Int) {
    val pq = new PQ(fold)
    // for(candidate <- candidates) pq.push(candidate)
    val len = candidates.length
    var i = 0
    while(i < len){
      pq.push(candidates(i))
      i += 1
    }
    pq.normalize
    while(!pq.empty){
      val rhs = pq.top
      if(!rhs.merged) merge_in(rhs)
      pq.pop
    }
  }

  def merge_in_pq(pq: PQ) {
    pq.normalize
    while(!pq.empty){
      print("+")
      val rhs = pq.top
      if(!rhs.merged)
        this.merge_in(rhs)
      pq.pop
    }
  }

  def merge_in(rhs: ACluster){
    rhs.merged = true
    rhs.valid = false
    // members ++= rhs.members
    members.insertAll(num_members, rhs.members)
    num_members += rhs.num_members
    rhs.num_members = 0
  }

  object ColLT_RM {
    var col:Int = -1
    def set(c:Int) {col = c}
    def apply(l:Int, r:Int) = data(l+col) < data(r+col)
  }

  def insertion_sort (array:Vector[Int], first:Int, last:Int) {
    var current = first + 1
    while (current < last) {
      val tmp = array(current)
      var i = current
      var tmp1 = array(i-1)
      while(ColLT_RM(tmp, tmp1) && first<i-1){
        array(i) = tmp1
        i -= 1
        tmp1 = array(i - 1)
      }
      if(ColLT_RM(tmp,tmp1)){
        array(i-1) = tmp
        array(i)   = tmp1
      }
      else{
        array(i) = tmp
      }
      current += 1
    }
  }

  def quickPartition(array:Vector[Int], first:Int, last:Int):Int = {
    var ff = first
    var ll = last
    var f  = array(ff)
    var l  = array(ll - 1)
    var pivot = array(ff + (ll-ff)/2)

    if (ColLT_RM(pivot,f)) {
      if (ColLT_RM(f,l))  pivot = f
      else if (ColLT_RM(pivot,l))  pivot = l
    }
    else if (ColLT_RM(l,f))  pivot = f
    else if (ColLT_RM(l,pivot))  pivot = l

    ff -= 1
    while (true) {
      ff += 1
	    while (ColLT_RM(array(ff), pivot)){ ff+=1 }
      ll -= 1
      while (ColLT_RM(pivot, array(ll))){ ll-=1 }
      if (ff >= ll)  return ff
      val tmp = array(ff)
      array(ff) = array(ll)
      array(ll) = tmp
    }
    ff
  }

  def nth_element(array:Vector[Int], f:Int, nth:Int, l:Int){
    var first = f
    var last  = l
    while (last - first > 3) {
	    val cut = quickPartition(array, first, last);
	    if (cut <= nth)  first = cut;
	    else  last = cut;
      }
    insertion_sort(array, first, last);
  }

}


