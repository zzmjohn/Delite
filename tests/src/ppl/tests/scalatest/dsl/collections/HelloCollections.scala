package ppl.tests.scalatest.dsl.collections



import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.collections._
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._



trait HashMapMap extends CollectionsApplication with DeliteTestModule {
  def main() {
    implicit val collector = ArrayBuffer[Boolean]()
    
    val sz = 60
    val ms = HashMap.range(sz)
    val ms2 = ms.map(kv => kv)
    //println(ms2)
    collect(ms2.size == sz)
    for (i <- 0 until sz) collect(ms2.get(i) == i)
    
    //println(HashMap.range(0))
    val hm0 = HashMap.range(0).map(kv => kv)
    collect(hm0.size == 0)
    
    val hm1 = HashMap.range(1).map(kv => kv)
    collect(hm1.size == 1)
    for (k <- 0 until 1) collect(hm1.get(k) == k)
    
    val hm2 = HashMap.range(2).map(kv => kv)
    collect(hm2.size == 2)
    for (k <- 0 until 2) collect(hm2.get(k) == k)
    
    val hm5 = HashMap.range(5).map(kv => kv)
    collect(hm5.size == 5)
    for (k <- 0 until 5) collect(hm5.get(k) == k)
    
    val hm10 = HashMap.range(10).map(kv => kv)
    collect(hm10.size == 10)
    for (k <- 0 until 10) collect(hm10.get(k) == k)
    
    val hm15 = HashMap.range(15).map(kv => kv)
    collect(hm15.size == 15)
    for (k <- 0 until 15) collect(hm15.get(k) == k)
    
    val hm150 = HashMap.range(150).map(kv => kv)
    collect(hm150.size == 150)
    for (k <- 0 until 150) collect(hm150.get(k) == k)
    
    val hm256 = HashMap.range(256).map(kv => kv)
    collect(hm256.size == 256)
    for (k <- 0 until 256) collect(hm256.get(k) == k)
    
    val hm300 = HashMap.range(300).map(kv => kv)
    collect(hm300.size == 300)
    for (k <- 0 until 300) collect(hm300.get(k) == k)
    
    val hm15000 = HashMap.range(15000).map(kv => kv)
    collect(hm15000.size == 15000)
    for (k <- 0 until 15000) collect(hm15000.get(k) == k)
    
    mkReport
  }
}


object HashMapMapRunner extends CollectionsApplicationRunner with HashMapMap with DeliteTestRunner


trait ArraySeqGroupBy extends CollectionsApplication with DeliteTestModule {
  def main() {
    implicit val collector = ArrayBuffer[Boolean]()
    
    val sz = 6
    val xs = ArraySeq.range(6)
    val mg = xs.groupBy(x => (x % 2, x))
    collect(mg.size == 2)
    for (k <- 0 until 2) {
      val b = mg.get(k)
      dcToDcOps(b).size
      collect(b.size == 3)
      for (i <- 0 until 3) collect(b(i) == (2 * i + k))
    }
    
    mkReport
  }
}


object ArraySeqGroupByRunner extends CollectionsApplicationRunner with ArraySeqGroupBy with DeliteTestRunner


class HelloCollectionsOpsSuite extends DeliteSuite {
  def testHashMapMap() { compileAndTest(HashMapMapRunner) }
  def testArraySeqGroupBy() { compileAndTest(ArraySeqGroupByRunner) }
}





