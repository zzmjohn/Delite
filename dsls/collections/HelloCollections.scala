



import ppl.delite.framework.collections._



// HashMap.fill[Int, String](xs)(x => x.toString)
trait HelloCollections extends CollectionsApplication {
  def main() {
    // val xs = ArraySeq[Int](6)
    // println("hello array: " + xs.size)
    // println("Now printing in foreach.")
    // for (x <- xs) println(x)
    // println("Now mapping and printing in foreach.")
    // val ys = xs.map(_ + 1)
    // for (y <- ys) println(y)
    // println("Now some filtering.")
    // val zs = ys.filter(_ % 2 == 1)
    // println("Elements left after filter: " + zs.size)
    // for (z <- zs) println(z)
    
    // xs.map({ x => (x, x) })(hashMapCanBuild, manifest[(Int, Int)], manifest[HashMapImpl[Int, Int]])
    //val ms = HashMap[Int, String]()
    val ms = HashMap.range(10)
    println(ms)
    val ms2 = ms.map(kv => kv)
    println(ms2)
    println(ms2.get(5))
    
    val xs = ArraySeq[Int](6)
    xs.groupBy(x => (x, x))
    println(xs)
  }
}


object HelloCollectionsRunner extends CollectionsApplicationRunner with HelloCollections





