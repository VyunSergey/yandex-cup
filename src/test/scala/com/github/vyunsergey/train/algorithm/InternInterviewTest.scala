package com.github.vyunsergey.train.algorithm

import com.github.vyunsergey.train.algorithm.InternInterview.FancySequence
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InternInterviewTest extends AnyFlatSpec with Matchers {

  "FancySequence" should "correctly create" in {
    val lst = List(1, 1).map(_.toLong)
    val fsq = FancySequence(lst)

    println(fsq)
    println(FancySequence(Nil).first)
    println(FancySequence(Nil).iteration(0))
    println(FancySequence(Nil).iteration(1))

    fsq.toList.zip(lst).foreach({case (e1, e2) => e1 shouldBe e2})
    fsq shouldBe FancySequence(Nil).first
  }
/*
  "FancySequence" should "correctly construct next" in {
    def checkSequence(actual: List[Long], expected: List[Long]): Unit = {
      actual.zip(expected).foreach({case (act, exp) => act shouldBe exp})
    }

    val fsq = FancySequence(Nil).first
    checkSequence(fsq.toList, List(1, 1).map(_.toLong))
    checkSequence(fsq.next.toList, List(1, 2, 1).map(_.toLong))
    checkSequence(fsq.next.next.toList, List(1, 3, 2, 3, 1).map(_.toLong))
    checkSequence(fsq.next.next.next.toList, List(1, 4, 3, 5, 2, 5, 3, 4, 1).map(_.toLong))
  }

  "FancySequence" should "correctly iterate" in {
    def checkSequence(actual: List[Long], expected: List[Long]): Unit = {
      actual.zip(expected).foreach({case (act, exp) => act shouldBe exp})
    }

    val fsq = FancySequence(Nil).first
    checkSequence(fsq.toList, List(1, 1))
    checkSequence(fsq.iteration(1).toList, List(1, 2, 1).map(_.toLong))
    checkSequence(fsq.iteration(2).toList, List(1, 3, 2, 3, 1).map(_.toLong))
    checkSequence(fsq.iteration(3).toList, List(1, 4, 3, 5, 2, 5, 3, 4, 1).map(_.toLong))
    checkSequence(fsq.iteration(4).toList, List(1, 5, 4, 7, 3, 8, 5, 7, 2, 7, 5, 8, 3, 7, 4, 5, 1).map(_.toLong))
    checkSequence(fsq.iteration(5).toList, List(1, 6, 5, 9, 4, 11, 7, 10, 3, 11, 8, 13, 5, 12, 7, 9, 2,
      9, 7, 12, 5, 13, 8, 11, 3, 10, 7, 11, 4, 9, 5, 6, 1).map(_.toLong))
  }

  "FancySequence" should "correctly iterate with big numbers" in {
    val fsq = FancySequence(Nil).first

    val res = fsq.iteration(1)
    // fsq.iteration(1000L)
    // fsq.iteration(1000000L)
    // fsq.iteration(1000000000L)
    // fsq.iteration(1000000000000L)

    println(res.toList)
  }

*/
}
