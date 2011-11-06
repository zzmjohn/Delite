package ppl.tests.scalatest.dsl.collections



import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.collections._
import ppl.dsl.optiml.{OptiMLApplicationRunner, OptiMLApplication}
import ppl.tests.scalatest._



trait Tuples extends CollectionsApplication with DeliteTestModule {
  def main() {
    implicit val collector = ArrayBuffer[Boolean]()
    
    val t = ArraySeq.range(5).map(x => (x, x))
    collect(t == t)
    
    mkReport
  }
}


object TuplesRunner extends CollectionsApplicationRunner with Tuples with DeliteTestRunner


class TuplesOpsSuite extends DeliteSuite {
  def testTuples() { compileAndTest(TuplesRunner) }
}

