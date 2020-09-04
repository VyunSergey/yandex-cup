package com.github.vyunsergey.train.algorithm

import scala.annotation.tailrec

object PalindromicString {

  def main(args: Array[String]): Unit = {
    while(true) {
      Console.out.println("Input string in format: '{string}'")
      Console.in.readLine() match {
        case "exit" => return
        case str: String =>
          PalindromicString(str).minPalindromicSubstring(2) match {
            case Some(s) => Console.out.println(s)
            case None => Console.out.println(-1)
          }
      }
    }
  }

  case class PalindromicString(str: String) {
    def substringsFiltered(len: Int)(condition: String => Boolean): List[String] = {
      @tailrec
      def innerSubstrings(len: Int, cond: String => Boolean, str: String, res: List[String]): List[String] = {
        if (str.length < len) res
        else if (cond(str.slice(0, len))) innerSubstrings(len, cond, str.tail, res :+ str.slice(0, len))
        else innerSubstrings(len, cond, str.tail, res)
      }
      innerSubstrings(len, condition, str, Nil)
    }

    def substrings(len: Int): List[String] = {
      substringsFiltered(len)(_ => true)
    }

    def minPalindromicSubstring(len: Int): Option[String] = {
      @tailrec
      def innerMinPalindromicSubstring(len: Int, str: String, res: Option[String]): Option[String] = {
        if (res.isDefined || str.length < len) res
        else if (substringsFiltered(len)(str => str == str.reverse).nonEmpty)
          substringsFiltered(len)(str => str == str.reverse).headOption
        else innerMinPalindromicSubstring(len + 1, str, res)
      }
      innerMinPalindromicSubstring(len, str, None)
    }
  }
}
