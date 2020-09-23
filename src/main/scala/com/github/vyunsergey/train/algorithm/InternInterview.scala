package com.github.vyunsergey.train.algorithm

import scala.annotation.tailrec

object InternInterview {

  case class FancySequence(list: List[Long]) {
    def first: FancySequence = FancySequence(List(1, 1))
    def next: FancySequence = {
      @tailrec
      def innerNext(list: List[Long], res: List[Long]): List[Long] = {
        println(s"NEXT list: $list, res: $res")
        list match {
          case a :: b :: tail => innerNext(tail, res ++ List(a, a + b, b))
          case _ => res
        }
      }
      FancySequence(innerNext(list, Nil))
    }
    def iteration(len: Long): FancySequence = {
      @tailrec
      def innerIteration(len: Long, res: List[Long]): List[Long] = {
        println(s"ITERATION len: $len, res: $res")
        if (len <= 0) res
        else innerIteration(len - 1, FancySequence(res).next.toList)
      }
      FancySequence(innerIteration(len, list))
    }
    def toList: List[Long] = list
  }
}
