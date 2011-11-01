



import ppl.delite.framework.collections._



// HashMap.fill[Int, String](xs)(x => x.toString)
trait HelloCollections extends CollectionsApplication {
  def main() {
    // val xs = ArraySeq.range(6).map(x => x)
    // println(xs)
    // val ys = ArraySeq.range(6).filter(x => x % 2 == 0)
    // println(ys)
    // val ms = HashMap.range(6).map(x => x)
    // println(ms)
    
    val mg = ArraySeq.range(6).groupBy(x => x % 2)
    println(mg)
    
    // val xs = ArraySeq.range(250000)
    
    // tic(xs)
    // val ys = xs.map(x => x)
    // toc(ys)
    
    // tic(ys)
    // val ys1 = xs.map(x => x)
    // toc(ys1)
    
    // tic(ys1)
    // val ys2 = xs.map(x => x)
    // toc(ys2)
  }
}


object HelloCollectionsRunner extends CollectionsApplicationRunner with HelloCollections





