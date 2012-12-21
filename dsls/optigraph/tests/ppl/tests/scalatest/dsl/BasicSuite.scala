package ppl.tests.scalatest.dsl.optigraph

import ppl.dsl.optigraph._
import ppl.dsl.optigraph.{OptiGraphApplication, OptiGraphApplicationRunner}
import ppl.tests.scalatest._

object DirectedGraphRunner extends DeliteTestRunner with OptiGraphApplicationRunner with DirectedGraph
trait DirectedGraph extends DeliteTestModule with OptiGraphApplication {
  def main() = {
    //Create an instance of a directed mutable graph
    val mg = DMutableGraph()
    //Add some nodes
    val mn1 = mg.AddNode
    val mn2 = mg.AddNode
    //Add some edges
    val me1 = mg.AddEdge(mn1, mn2)
    val me2 = mg.AddEdge(mn1, mn2) //this one is a repeated edge
    val me3 = mg.AddEdge(mn2, mn1)
    val me4 = mg.AddEdge(mn1, mn1) //this one is a self-edge
    //Create an immutable snapshot of the mutable graph
    val g = mg.Snapshot

    collect(g.NumNodes == 2)
    collect(g.NumEdges == 4)

    collect(g.NumNodes == mg.NumNodes)
    collect(g.NumEdges == mg.NumEdges)

    // TODO Find a new way to test "getNode" and "getEdge" since we're using Mutable Edges/Nodes
    /*
    if(g.Node(n1.Id).Id != n1.Id) {
      println("[FAIL] Wrong node returned")
    } else {
      println("[OK] Correct node returned")
    }
    if(g.Edge(e1.Id).Id != e1.Id) {
      println("[FAIL] Wrong edge returned")
    } else {
      println("[OK] Correct edge returned")
    }
    */

    val nodes = g.Nodes.toSet
    val edges = g.Edges.toSet
    collect(nodes.Size == 2)
    collect(edges.Size == 4)

    // TODO Find a new way to test GSet "Has"
    /*
    if((!nodes.Has(n1)) || (!nodes.Has(n2)) || (!edges.Has(e1)) || (!edges.Has(e2))
        || (!edges.Has(e3)) || (!edges.Has(e4))) {
      println("[FAIL] Wrong nodes/edges collection contents")
    } else {
      println("[OK] Correct nodes/edges collection contents")
    }
    */

    //---------//

    //Create an instance of a directed mutable graph
    val mg2 = DMutableGraph()
    //Add some nodes
    val mn3 = mg2.AddNode
    val mn4 = mg2.AddNode
    //Add some edges
    val me5 = mg2.AddEdge(mn3, mn4)
    val me6 = mg2.AddEdge(mn3, mn4)
    val me7 = mg2.AddEdge(mn4, mn3)
    val me8 = mg2.AddEdge(mn3, mn3)
    //Create an immutable snapshot of the mutable graph
    val g_s = mg2.Snapshot

    collect(g_s.Node(0).Degree == 3 || g_s.Node(1).Degree == 3)
    collect(g_s.Node(0).Degree == 1 || g_s.Node(1).Degree == 1)

    mkReport
  }
}

class BasicSuite extends DeliteSuite {
  def testDirectedGraph() { compileAndTest(DirectedGraphRunner) }
}

