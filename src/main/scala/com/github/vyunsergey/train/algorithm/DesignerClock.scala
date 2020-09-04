package com.github.vyunsergey.train.algorithm

import scala.annotation.tailrec
import scala.util.Try

object DesignerClock {

  def main(args: Array[String]): Unit = {
    while(true) {
      Console.out.println(
        """Input Clock time in format: '{hour} {minute}'
          |  {hour}   should be in range [1, 2, ... 12]
          |  {minute} should be in range [0, 1, ... 59]
          |""".stripMargin)
      Console.in.readLine().split(" ").toList match {
        case h :: m :: _ =>
          val result: Option[Clock] =
            for {
              hour <- Try(h.trim.toInt).toOption
              minute <- Try(m.trim.toInt).toOption
            } yield Clock(0).fromHourMinute(Hour(hour), Minute(minute)).verticalMirror
          result match {
            case Some(clock) =>
              val (hour, minute) = clock.toHourMinute
              Console.out.println(s"${hour.toInt} ${minute.toInt}")
            case None =>
              Console.out.println(s"Error: Wrong input format: $h $m")
          }
        case "exit" :: Nil =>
          return
        case lst =>
          Console.out.println(s"Error: Wrong input format: ${lst.mkString(" ")}")
      }
    }
  }

  trait Time {
    @tailrec
    final def periodic(start: Int, end: Int)(value: Int): Int = {
      if (value > end) periodic(start, end)(value - (end - start + 1))
      else if (value < start) periodic(start, end)(value + (end - start + 1))
      else value
    }

    def update(value: Int): Time
    def add(value: Int): Time
    def next: Time
    def toInt: Int

    override def equals(obj: Any): Boolean = obj match {
      case t: Time => toInt == t.toInt
      case _ => false
    }
  }

  case class Hour(from: Int) extends Time {
    def update(value: Int): Hour = Hour(value)
    def add(value: Int): Hour = Hour(from + value)
    def next: Hour = add(1)
    def toInt: Int = periodic(1, 12)(from)

    def centralMirror: Hour = Hour(from + 6)
    def horizontalMirror: Hour = Hour(6 - from)
    def verticalMirror: Hour = Hour(12 - from)

    override def toString: String = s"${this.toInt} hour"
  }

  case class Minute(from: Int) extends Time {
    def update(value: Int): Minute = Minute(value)
    def add(value: Int): Minute = Minute(from + value)
    def next: Minute = add(1)
    def toInt: Int = periodic(0, 59)(from)

    def centralMirror: Minute = Minute(from + 30)
    def horizontalMirror: Minute = Minute(30 - from)
    def verticalMirror: Minute = Minute(60 - from)

    override def toString: String = s"${this.toInt} minute"
  }

  case class Clock(from: Int) extends Time {
    def update(value: Int): Clock = Clock(value)
    def add(value: Int): Clock = Clock(from + value)
    def next: Clock = add(1)
    def toInt: Int = periodic(0, 11 * 60 + 59)(from)

    def fromHourMinute(hour: Hour, minute: Minute): Clock = {
      Clock(hour.toInt * 60 + minute.toInt)
    }

    def toHourMinute: (Hour, Minute) = {
      val time = toInt
      val minute = time % 60
      val hour = (time - minute) / 60
      (Hour(hour), Minute(minute))
    }

    def centralMirror: Clock = {
      val (hour, minute) = toHourMinute
      fromHourMinute(hour.centralMirror, minute.centralMirror)
    }

    def horizontalMirror: Clock = {
      val (hour, minute) = toHourMinute
      fromHourMinute(hour.horizontalMirror, minute.horizontalMirror)
    }

    def verticalMirror: Clock = {
      val (hour, minute) = toHourMinute
      fromHourMinute(hour.verticalMirror, minute.verticalMirror)
    }

    override def toString: String = {
      val (hour, minute) = toHourMinute
      s"$hour $minute"
    }
  }
}
