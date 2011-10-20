package ppl.tests.scalatest.dsl.collections



import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.collections._
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._



trait HelloCollections extends CollectionsApplication with DeliteTestModule {
  def main() {
    implicit val collector = ArrayBuffer[Boolean]()
    
    val ms = HashMap.range(60)
    val ms2 = ms.map(kv => kv)
    //println(ms2)
    
    mkReport
  }
}


object HelloCollectionsRunner extends CollectionsApplicationRunner with HelloCollections with DeliteTestRunner


class HelloCollectionsOpsSuite extends DeliteSuite {
  def testHelloCollections() { compileAndTest(HelloCollectionsRunner) }
}



