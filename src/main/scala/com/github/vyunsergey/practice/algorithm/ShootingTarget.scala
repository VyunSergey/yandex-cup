package com.github.vyunsergey.practice.algorithm

object ShootingTarget {
  import Point._

  def main(args: Array[String]): Unit = {
    val numTargets: Int = Console.in.readLine().toInt
    val lines: List[String] = 0.until(numTargets).toList.map(_ => Console.in.readLine())
    val targets: List[CentredFigure] = lines.map { line =>
      val numbers: List[Double] = line.split(" ").toList.map(_.toDouble)
      if (numbers.head == 0) Circle.zero.fromList(numbers.tail) else Rectangle.zero.fromList(numbers.tail)
    }
    if (targets.length <= 2) println("Yes")
    else {
      val line: Line = Line(targets.head.center, targets.tail.head.center)
      val lineFlag: Boolean = targets.forall(figure => line.contains(figure.center))
      if (lineFlag) println("Yes") else println("No")
    }
  }

  case class Point(x: Double, y: Double)
  object Point {
    def zero: Point = Point(0.0, 0.0)
    def apply(str: String): Point = {
      val xs = str.split(" ").map(_.toDouble)
      Point(xs.head, xs.tail.head)
    }
    def apply(xs: List[Double]): Point = {
      Point(xs.head, xs.tail.head)
    }

    implicit class PointOps(point: Point) {
      def +(that: Point): Point = Point(point.x + that.x, point.y + that.y)
      def -(that: Point): Point = Point(point.x - that.x, point.y - that.y)
      def <*>(that: Point): Double = point.x * that.x + point.y * that.y
      def *(that: Point): Point = Point(point.x * that.x, point.y * that.y)
      def *(lambda: Double): Point = Point(point.x * lambda, point.y * lambda)
      def </>(that: Point): Double = point.x * that.y - point.y * that.x
      def /(that: Point): Point = Point(point.x / that.x, point.y / that.y)
      def /(lambda: Double): Point = Point(point.x / lambda, point.y / lambda)
    }
  }

  case class Line(p0: Point, p1: Point) {
    def contains(p: Point): Boolean = {
      (p - p0) </> (p1 - p0) == 0
    }
  }

  trait CentredFigure {
    def center: Point
  }

  case class Circle(radius: Double, center: Point) extends CentredFigure {
    def fromString(str: String): Circle = {
      val xs = str.split(" ").map(_.toDouble)
      val radius = xs.head
      val center = Point(xs.tail.head, xs.tail.tail.head)
      Circle(radius, center)
    }
    def fromList(xs: List[Double]): Circle = {
      Circle(xs.head, Point(xs.tail))
    }
  }
  object Circle {
    def zero: Circle = Circle(0.0, Point.zero)
  }

  case class Rectangle(p00: Point,
                       p01: Point,
                       p10: Point,
                       p11: Point) extends CentredFigure {
    def center: Point = (p00 + p01 + p10 + p11) / 4.0
    def fromString(str: String): Rectangle = {
      val xs = str.split(" ").map(_.toDouble)
      val xs1 = xs.tail.tail
      val xs2 = xs1.tail.tail
      val xs3 = xs2.tail.tail
      val p00 = Point(xs.head, xs.tail.head)
      val p01 = Point(xs1.head, xs1.tail.head)
      val p10 = Point(xs2.head, xs2.tail.head)
      val p11 = Point(xs3.head, xs3.tail.head)
      Rectangle(p00, p10, p01, p11)
    }
    def fromList(xs: List[Double]): Rectangle = {
      val xs1 = xs.tail.tail
      val xs2 = xs1.tail.tail
      val xs3 = xs2.tail.tail
      val p00 = Point(xs.head, xs.tail.head)
      val p01 = Point(xs1.head, xs1.tail.head)
      val p10 = Point(xs2.head, xs2.tail.head)
      val p11 = Point(xs3.head, xs3.tail.head)
      Rectangle(p00, p10, p01, p11)
    }
  }
  object Rectangle {
    def zero: Rectangle = Rectangle(Point.zero, Point.zero, Point.zero, Point.zero)
  }
}
