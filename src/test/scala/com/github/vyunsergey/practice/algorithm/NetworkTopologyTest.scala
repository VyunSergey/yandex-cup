package com.github.vyunsergey.practice.algorithm

import com.github.vyunsergey.practice.algorithm.NetworkTopology.Graph
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.{TreeMap, TreeSet}

class NetworkTopologyTest extends AnyFlatSpec with Matchers {
  val planarEdges: TreeMap[Int, TreeSet[Int]] = TreeMap(
    1->TreeSet(2),
    2->TreeSet(1, 3),
    3->TreeSet(2, 4),
    4->TreeSet(3, 5),
    5->TreeSet(4)
  )
  val planarGraph: Graph = Graph(planarEdges)

  val cyclicEdges: TreeMap[Int, TreeSet[Int]] = TreeMap(
    1->TreeSet(2, 5),
    2->TreeSet(1, 3),
    3->TreeSet(2, 4),
    4->TreeSet(3, 5),
    5->TreeSet(1, 4)
  )
  val cyclicGraph: Graph = Graph(cyclicEdges)

  val starEdges: TreeMap[Int, TreeSet[Int]] = TreeMap(
    1->TreeSet(2, 3, 4, 5),
    2->TreeSet(1),
    3->TreeSet(1),
    4->TreeSet(1),
    5->TreeSet(1)
  )
  val starGraph: Graph = Graph(starEdges)

  val complexEdges: TreeMap[Int, TreeSet[Int]] = TreeMap(
    1->TreeSet(2, 4),
    2->TreeSet(1, 3),
    3->TreeSet(2, 4),
    4->TreeSet(1, 3, 5),
    5->TreeSet(4, 6),
    6->TreeSet(5, 7, 9),
    7->TreeSet(6, 8),
    8->TreeSet(7, 9),
    9->TreeSet(6, 8)
  )
  val complexGraph: Graph = Graph(complexEdges)

  val allGraphs: List[Graph] = List(planarGraph, cyclicGraph, starGraph, complexGraph)

  "Graph" should "correctly construct" in {
    def checkSize(graph: Graph, expected: Int, verbose: Boolean = false): Unit = {
      if (verbose) {
        graph.printGraph()
        println(s"Graph length: ${graph.length} Expected: $expected")
      }
      graph.length shouldBe expected
    }

    allGraphs.foreach(g => checkSize(g, g.edges.map(_._2.size).sum))
  }

  it should "correctly work with nodes" in {
    def checkNodes(graph: Graph, expected: TreeSet[Int], verbose: Boolean = false): Unit = {
      if (verbose) {
        graph.printGraph()
        println(s"Graph nodes: ${graph.nodes().mkString("[", ",", "]")} Expected: ${expected.mkString("[", ",", "]")}")
      }
      graph.nodes shouldBe expected
    }

    allGraphs.foreach { g =>
      val outKeys = g.edges.keySet
      val inKeys = g.edges.flatMap{case (k, v) => v.map((_, k))}.keySet
      checkNodes(g, outKeys ++ inKeys)
    }
  }

  it should "correctly work with neighbors" in {
    def checkNeighbors(graph: Graph, node: Int, expected: TreeSet[Int], verbose: Boolean = false): Unit = {
      if (verbose) {
        graph.printGraph()
        println(s"Graph node: $node neighbors: ${graph.neighbors(node).mkString("[", ",", "]")} Expected: ${expected.mkString("[", ",", "]")}")
      }
      graph.neighbors(node) shouldBe expected
    }

    allGraphs.foreach { g =>
      g.nodes().foreach { i =>
        val outNodes: TreeSet[Int] = TreeSet(g.edges.filter(_._1 == i).values.flatten.toSeq: _*)
        val inNodes: TreeSet[Int] = g.edges.filter(_._2.contains(i)).keySet
        checkNeighbors(g, i, inNodes ++ outNodes)
      }
    }
  }

  it should "correctly work with allPaths" in {
    def checkAllPaths(graph: Graph, verbose: Boolean = false): Unit = {
      if (verbose) graph.printGraph()
      graph.nodes().foreach { node1 =>
        graph.allPathsDistances(node1).foreach { case (node2, (path, _)) =>
          if (node1 == node2) path.length shouldBe 0
          else {
            val beLengthCheck = be >= 1 and be <= graph.nodes().size
            if (verbose) println(s"Path (from: $node1 to: $node2): $path")
            path.length should beLengthCheck
            path.edges.exists{case (i, vec) => i == node1 || vec.contains(node1)} shouldBe true
            path.edges.exists{case (i, vec) => i == node2 || vec.contains(node2)} shouldBe true
          }
        }
      }
    }

    allGraphs.foreach { g =>
      checkAllPaths(g)
    }
  }

  it should "correctly work with safetyNetwork" in {
    def checkSafetyNetwork(graph: Graph, verbose: Boolean = false): Unit = {
      val (node1, node2, dist) = graph.safetyNetwork()
      if (verbose) {
        graph.printGraph()
        println(s"Safety Nodes: ($node1, $node2) with Dist: $dist")
      }
      node1 != node2 shouldBe true
      graph.nodes().intersect(Set(node1, node2)) shouldBe TreeSet(node1, node2)
      val beLengthCheck = be >= 1 and be <= graph.nodes().size
      dist should beLengthCheck
    }

    allGraphs.foreach { g =>
      checkSafetyNetwork(g)
    }
  }
}
