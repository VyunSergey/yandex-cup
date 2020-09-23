package com.github.vyunsergey.practice.algorithm

import com.github.vyunsergey.train.algorithm.PalindromicString.PalindromicString

object PalindromicFactor {
  def main(args: Array[String]): Unit = {
    val str: String = Console.in.readLine()
    val palindromicStr = PalindromicString(str).minPalindromicSubstring(2)
    palindromicStr match {
      case Some(str) => println(str)
      case None => println("-1")
    }
  }
}
