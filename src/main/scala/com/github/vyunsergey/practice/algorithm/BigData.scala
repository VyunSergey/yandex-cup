package com.github.vyunsergey.practice.algorithm

import scala.collection.immutable.TreeMap

object BigData {
  def main(args: Array[String]): Unit = {
    val Array(n, m) = Console.in.readLine().split(" ").map(_.toLong).take(2)
    val aNumbers = Numbers(Console.in.readLine().split(" ").map(_.toLong).toList)
    val bNumbers = Numbers(Console.in.readLine().split(" ").map(_.toLong).toList)
    println(s"$n, $m")
    val table = Table[Long]((a: Long, b: Long) => b * 1e9.toLong + a)(aNumbers, bNumbers)
    table.print()
    println(table.findPath(_ + _).map(_._2).sum)
  }

  case class Numbers[A](list: List[A]) {
    def length: Int = list.length
    def map[U](f: A => U): Numbers[U] = Numbers(list.map(f))
    def flatMap[U](f: A => Numbers[U]): Numbers[U] = Numbers(list.flatMap(e => f(e).list))
    def foreach[U](f: A => U): Unit = list.foreach(f)
    def zipWithIndex: Numbers[(A, Int)] = Numbers(list.zipWithIndex)
    def reduce[B >: A](f: (B, B) => B): B = list.reduce(f)
  }
  object Numbers {
    def apply[A](f: String => A)(str: String): Numbers[A] = {
      Numbers(str.split(" ").toList.map(f))
    }
  }

  type TreeTable[A] = TreeMap[Int, TreeMap[Int, A]]

  case class Table[A: Ordering](values: TreeTable[A]) {
    def lengthN: Int = values.values.map(_.size).max
    def lengthM: Int = values.size
    def length: Int = lengthN * lengthM
    def index(i: Int, j: Int): A = values.get(j).flatMap(_.get(i)).get

    def print(): Unit = println(values.values
      .map(_.values.map(a => f"$a%3s").mkString("[", ",", "]")).mkString("\n"))

    def findPath(f: (A, A) => A): List[((Int, Int), A)] = {
      def findPathRec(i: Int, j: Int, f: (A, A) => A): List[((Int, Int), A)] = {
        if (i == 0 && j == 0) List(((i, j), index(i, j)))
        else if (i == 0) ((i, j), index(i, j)) +: findPathRec(i, j - 1, f)
        else if (j == 0) ((i, j), index(i, j)) +: findPathRec(i - 1, j, f)
        else {
          val leftPath = findPathRec(i - 1, j, f)
          val left = leftPath.map(_._2).reduce(f)
          val upPath = findPathRec(i, j - 1, f)
          val up = upPath.map(_._2).reduce(f)
          val maxPath = if (Ordering[A].compare(left, up) > 0) leftPath else upPath
          ((i, j), index(i, j)) +: maxPath
        }
      }
      findPathRec(lengthN - 1, lengthM - 1, f)
    }
  }

  object Table {
    def apply[A: Ordering](f: (A, A) => A)(aNumbers: Numbers[A], bNumbers: Numbers[A]): Table[A] = {
      val values: TreeTable[A] = TreeMap(bNumbers.zipWithIndex.map { case (a, i) =>
        i -> TreeMap(aNumbers.zipWithIndex.map { case (b, j) =>
            j -> f(a, b)
          }.list: _*)
      }.list: _*)
      Table(values)
    }
  }
}
