package ppl.tests.scalatest.dsl.collections



import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.collections._
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._



trait HelloCollections extends CollectionsApplication {
  def main() {
    val xs = ArraySeq[Int](6)
    for (x <- xs) println(x)
    val ys = xs.map(_ + 1)
    for (y <- ys) println(y)
    val zs = ys.filter(_ % 2 == 1)
    for (z <- zs) println(z)
    for (x <- xs) {
      val ts = ys.filter(_ % 2 == (x + 1))
      println(ts)
    }
  }
}


object HelloCollectionsRunner extends CollectionsApplicationRunner with HelloCollections // with DeliteTestRunner


class HelloCollectionsOpsSuite extends DeliteSuite {
  //def testHelloCollections() { compileAndTest(HelloCollectionsRunner) }
}



