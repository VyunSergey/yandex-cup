package com.github.vyunsergey.practice.algorithm

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.mutable.ArrayBuffer

object NetworkTopology {
  def main(args: Array[String]): Unit = {
    val n = Console.in.readLine().toInt
    val edges = 1.until(n).flatMap {_ =>
      val Array(i, j) = Console.in.readLine().split(" ").map(_.toInt).take(2)
      List((i, TreeSet(j)), (j, TreeSet(i)))
    }.foldLeft(TreeMap.empty[Int, TreeSet[Int]]) { case (map, (i, set)) =>
      val (key, value) = map.find(_._1 == i).map(e => (i, e._2 ++ set)).getOrElse((i, set))
      map.filter(_._1 != i).updated(key, value)
    }
    val graph = Graph(edges)
    graph.printGraph()
    val (node1, node2, _) = graph.safetyNetwork()
    println(s"$node1 $node2")
  }

  case class Graph(edges: TreeMap[Int, TreeSet[Int]]) {
    def length: Int = edges.map(_._2.size).sum
    def nodes(): TreeSet[Int] = edges.keySet
    def nodes(index: Int): Int = nodes().slice(index, index + 1).head
    def nodesIndex(node: Int): Int = nodes().zipWithIndex.filter(_._1 == node).map(_._2).head
    def neighbors(node: Int): TreeSet[Int] = edges(node)
    override def toString: String = {
      edges.map { case (k, vec) => vec.map(v => s"$k->$v").mkString(", ") }.mkString("[", ", ", "]")
    }
    def printGraph(): Unit = {
      println(s"Graph: $toString")
    }
    def allPathsDistances(node: Int): TreeMap[Int, (Graph, Int)] = {
      val length = nodes().size
      val dist: ArrayBuffer[Int] = ArrayBuffer.from(List.fill(length)(1e5.toInt))
      val paths: ArrayBuffer[Vector[Int]] = ArrayBuffer.from(List.fill(length)(Vector.empty[Int]))
      val mark: ArrayBuffer[Boolean] = ArrayBuffer.from(List.fill(length)(false))
      val nodeIndex = nodesIndex(node)
      dist(nodeIndex) = 0
      paths(nodeIndex) = Vector(node)
      while (mark.contains(false)) {
        val (j, _) = mark.zipWithIndex.filter(!_._1).map { case (_, i) =>
          (i, dist(i))
        }.minBy(_._2)
        mark(j) = true
        neighbors(nodes(j)).foreach { n =>
          val i = nodesIndex(n)
          if (!mark(i) && dist(i) > dist(j) + 1) {
            dist(i) = dist(j) + 1
            paths(i) = paths(j) :+ nodes(i)
          }
        }
      }

      TreeMap.from(nodes().toSeq.map { n =>
        val i = nodesIndex(n)
        val vec = paths(i)
        (n, (Graph(TreeMap.from(vec.zip(vec.tail.map(TreeSet(_))))), dist(i)))
      })
    }
    def path(node1: Int, node2: Int): Graph = allPathsDistances(node1)(node2)._1
    def distance(node1: Int, node2: Int): Int = allPathsDistances(node1)(node2)._2
    def safetyNetwork(): (Int, Int, Int) = {
      val length = nodes().size
      val distMatrix: ArrayBuffer[Int] = ArrayBuffer.from(List.fill(length * length)(0))
      nodes().foreach { n =>
        val i = nodesIndex(n)
        val dist = allPathsDistances(n)
        dist.foreach { case (m, (_, d)) =>
          val j = nodesIndex(m)
          distMatrix(i * length + j) = d
        }
      }
      /*
      distMatrix.zipWithIndex.foreach { case (d, i) =>
        if ((i + 1) % length == 0) println(f"$d%3d")
        else print(f"$d%3d ")
      }
      */

      val (node1, node2, dist) = nodes().flatMap { n =>
        val i = nodesIndex(n)
        nodes().filter(_ > n).map { m =>
          val j = nodesIndex(m)
          val dist = nodes().filter(!List(n, m).contains(_)).map { node =>
            val k = nodesIndex(node)
            Math.min(distMatrix(i * length + k), distMatrix(j * length + k))
          }.max
          (n, m, dist)
        }
      }.minBy(_._3)

      (node1, node2, dist)
    }
  }
}
