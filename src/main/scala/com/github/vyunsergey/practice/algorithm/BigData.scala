package com.github.vyunsergey.practice.algorithm

import scala.collection.immutable.HashMap

object BigData {

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

  type HashTable[A] = HashMap[Int, HashMap[Int, A]]

  case class Table[A](values: HashTable[A]) {
    def length: Int = values.size * values.values.map(_.size).max
    def index(i: Int, j: Int): A = values.get(i).flatMap(_.get(j)).get
  }
  object Table {
    def apply[A](f: (A, A) => A)(aNumbers: Numbers[A], bNumbers: Numbers[A]): Table[A] = {
      val values = (for {
        a <- aNumbers.zipWithIndex
        b <- bNumbers.zipWithIndex
      } yield {
        val (ai, i) = a
        val (bj, j) = b
        HashMap(i -> HashMap(j -> f(ai, bj)))
      }).reduce[HashTable[A]]({case (ht1, ht2) => ht1 ++ ht2})
      Table(values)
    }
  }
}
