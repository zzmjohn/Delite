package ppl.tests.scalatest.dsl.collections



import scala.virtualization.lms.common._
import java.io._
import ppl.delite.framework.collections._
import ppl.delite.framework.datastruct.scala._
import ppl.delite.framework.DeliteApplication
import ppl.tests.scalatest._
import ppl.delite.framework.datastruct.scala.{ArraySeqImpl, ArraySeq}



trait Constants {
  def pages: Array[String] = {
    val a = new collection.mutable.ArrayBuffer[String](10)
    val reader = new BufferedReader(new FileReader("dsls/collections/links-sorted-small.txt"))
    var line: String = reader.readLine
    while (line != null) {
      a += line
      line = reader.readLine
    }
    a.toArray
  }
}


trait ReverseWebLink extends CollectionsApplication with DeliteTestModule {
  val input: Rep[Array[String]] // TODO we don't use this, because input is empty now - why?
  
  def main() {
    implicit val collector = ArrayBuffer[Boolean]()
    
    // load data
    val reader = BufferedReader(FileReader("dsls/collections/links-sorted-small.txt"))
    val linesbuffer = ArrayBuffer[String]()
    var line = reader.readLine
    while (line != null) {
      linesbuffer += line
      line = reader.readLine
    }
    val pagelinks = ArraySeq.fromArrayBuffer(linesbuffer)
    
    // test flatMap to tuples
    val sdtuples = pagelinks flatMap {
      l =>
      val sourcedests = l.split(":")
      val source = Long.parseLong(sourcedests(0))
      val dests = sourcedests(1).trim.split(" ")
      ArraySeq.fromArray(dests).map(d => (source, Long.parseLong(d)))
    }
    // println(sdtuples)
    
    tic(pagelinks)
    // flatMap it
    val sourcedests = pagelinks flatMap {
      l =>
      val sourcedests = l.split(":")
      val source = Long.parseLong(sourcedests(0))
      val dests = sourcedests(1).trim.split(" ")
      ArraySeq.fromArray(dests).map(d => long_plus(source, Long.parseLong(d) << 32))
    }
    //println(sourcedests.size)
    //val sdtuples2 = sourcedests map {x => (x >>> 32, long_and(x, 0xffffffffL)) }
    //println(sdtuples2)
    
    // groupBy it
    val invertedAndShifted = sourcedests groupBy {
      x => long_and(x, 0xffffffff00000000L)
    }
    
    // map it properly
    val inverted = invertedAndShifted map {
      x => (x._1 >>> 32, x._2 map { src => long_and(src, 0xffffffffL) })
    }
    println(inverted)
    toc(inverted)
    
    // println(inverted)
    collect(1 == 1)
    
    mkReport
  }
}


object ReverseWebLinkRunner extends CollectionsApplicationRunner with Constants with ReverseWebLink with DeliteTestRunner with StaticDataExp {
  val input = staticData(pages) // doesn't work - input is empty - why?
}


class ReverseWebLinkOpsSuite extends DeliteSuite {
  def testReverseWebLink() {
    compileAndTest(ReverseWebLinkRunner)
  }
}








