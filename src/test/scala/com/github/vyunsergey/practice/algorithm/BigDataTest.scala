package com.github.vyunsergey.practice.algorithm

import com.github.vyunsergey.practice.algorithm.BigData.{Numbers, Table}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BigDataTest extends AnyFlatSpec with Matchers {
  "Numbers" should "correctly construct" in {
    val str: String = "1 2 3 4 5"
    val toInt: String => Int = _.toInt
    val numbers: Numbers[Int] = Numbers(toInt)(str)
    numbers.length shouldBe 5
  }

  it should "correctly work with zipWithIndex" in {
    val list: List[Int] = List(1, 1, 1, 1, 1)
    val numbers: Numbers[Int] = Numbers(list)
    val numbersInd: Numbers[(Int, Int)] = numbers.zipWithIndex
    numbersInd.length shouldBe 5
    numbersInd.map(_._2) shouldBe Numbers(list.zipWithIndex.map(_._2))
  }

  it should "correctly work with reduce" in {
    val list: List[Int] = List(1, 1, 1, 1, 1)
    val numbers: Numbers[Int] = Numbers(list)
    numbers.reduce(_ + _) shouldBe 5
  }

  "Table" should "correctly construct" in {
    val aNumbers: Numbers[Int] = Numbers(List(1, 2, 3, 4, 5))
    val bNumbers: Numbers[Int] = Numbers(List(1, 2, 3, 4, 5, 6, 7))
    val table: Table[Int] = Table[Int]((a: Int, b: Int) => a * 10 + b)(aNumbers, bNumbers)
    println(table)
  }

}
