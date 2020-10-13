package com.github.vyunsergey.practice.algorithm

import com.github.vyunsergey.practice.algorithm.BigData.{Numbers, Table}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BigDataTest extends AnyFlatSpec with Matchers {
  "Numbers" should "correctly construct" in {
    val str: String = "1 2 3 4 5"
    val toInt: String => Int = _.toInt
    val numbers: Numbers[Int] = Numbers(toInt)(str)
    numbers.length shouldBe str.split(" ").length
  }

  it should "correctly work with zipWithIndex" in {
    val list: List[Int] = List(1, 1, 1, 1, 1)
    val numbers: Numbers[Int] = Numbers(list)
    val numbersInd: Numbers[(Int, Int)] = numbers.zipWithIndex
    numbersInd.length shouldBe list.length
    numbersInd.map(_._2) shouldBe Numbers(list.zipWithIndex.map(_._2))
  }

  it should "correctly work with reduce" in {
    val list: List[Int] = List(1, 1, 1, 1, 1)
    val numbers: Numbers[Int] = Numbers(list)
    numbers.reduce(_ + _) shouldBe list.length
  }

  "Table" should "correctly construct" in {
    val aNumbers: Numbers[Int] = Numbers(List(1, 2, 3, 4, 5, 6, 7))
    val bNumbers: Numbers[Int] = Numbers(List(1, 2, 3, 4))
    val table: Table[Int] = Table[Int]((a: Int, b: Int) => a * 1e2.toInt + b)(aNumbers, bNumbers)
    table.print()
    table.lengthN shouldBe aNumbers.length
    table.lengthM shouldBe bNumbers.length
  }

  it should "correctly work with findPath" in {
    val aNumbers: Numbers[Int] = Numbers(List(1, 2, 3, 4, 5, 6, 7))
    val bNumbers: Numbers[Int] = Numbers(List(1, 2, 3, 4))
    val table: Table[Int] = Table[Int]((a: Int, b: Int) => a * 1e2.toInt + b)(aNumbers, bNumbers)
    table.print()
    val path: List[((Int, Int), Int)] = table.findPath(_ + _)
    path.map(_._2).sum shouldBe {
      val leftEdge = 0.until(table.lengthM - 1).map(table.index(0, _))
      val bottomEdge = 0.until(table.lengthN).map(table.index(_, table.lengthM - 1))
      (leftEdge ++ bottomEdge).sum
    }
  }

}
