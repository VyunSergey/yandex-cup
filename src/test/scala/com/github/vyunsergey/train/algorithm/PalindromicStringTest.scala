package com.github.vyunsergey.train.algorithm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.vyunsergey.train.algorithm.PalindromicString.PalindromicString

class PalindromicStringTest extends AnyFlatSpec with Matchers {

  implicit class SubstringsSyntax(str: String) {
    def substrings(len: Int): List[String] =
      str.toSeq.sliding(len, 1).toList.map(_.toString)
  }

  "PalindromicString" should "correctly return substrings" in {
    def checkSubstrings(str: String, len: Int): Unit = {
      PalindromicString(str)
        .substrings(len)
        .zip(str.substrings(len))
        .foreach({case (str, strExpected) => str shouldBe strExpected})
    }

    val str = "1234567890abcdefghijklmnopqrstuvwxyzuimyntbrvecwqsqwcv"
    2.to(100).map(checkSubstrings(str, _))
  }

  it should "correctly return filtered substrings" in {
    def checkFilteredSubstrings(str: String, len: Int, cond: String => Boolean,
                                expectedStrings: List[String]): Unit = {
      PalindromicString(str)
        .substringsFiltered(len)(cond)
        .zip(expectedStrings)
        .foreach({case (str, strExpected) => str shouldBe strExpected})
    }

    val str = "*************************"
    val cond = (str: String) => str == str.reverse
    2.to(100).map(i => checkFilteredSubstrings(str, i, cond, str.substrings(i)))
  }

  it should "correctly return palindromes" in {
    def checkPalindromes(str: String, len: Int, expectedPalindromes: List[String]): Unit = {
      PalindromicString(str)
        .substringsFiltered(len)(str => str == str.reverse)
        .zip(expectedPalindromes)
        .foreach({case (str, strExpected) => str shouldBe strExpected})
    }

    val str = "abbacbbdeedfghijklmn"
    checkPalindromes(str, 4, List("abba", "deed"))
    checkPalindromes(str, 2, List("bb", "bb", "ee"))
  }

  it should "correctly return minimal palindrome" in {
    def checkMinPalindrome(str: String, len: Int, expectedPalindrome: Option[String]): Unit = {
      PalindromicString(str)
        .minPalindromicSubstring(len) shouldBe expectedPalindrome
    }

    val str = "abbacbbdeedfghijklmn"
    checkMinPalindrome(str, 8, None)
    checkMinPalindrome(str, 4, Some("abba"))
    checkMinPalindrome(str, 2, Some("bb"))
  }
}
