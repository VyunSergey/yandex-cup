package com.github.vyunsergey.train.algorithm

import com.github.vyunsergey.train.algorithm.DesignerClock.{Clock, Hour, Minute}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DesignerClockTest extends AnyFlatSpec with Matchers {

  "Hour" should "correctly construct" in {
    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    hours.foreach(h => 1.to(12).contains(h.toInt) shouldBe true)
  }

  it should "have toInt method" in {
    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    hours.foreach(h => Hour(h.toInt) shouldBe h)
  }

  it should "have period 12" in {
    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    hours.foreach(h => h.add(12) shouldBe h)
  }

  it should "have next method" in {
    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    hours zip hours.tail foreach {case (h1, h2) => h1.next shouldBe h2}
  }

  it should "correctly mirror centrally" in {
    val testHour: List[(Hour, Hour)] = List(
      (Hour(1), Hour(7)),
      (Hour(2), Hour(8)),
      (Hour(3), Hour(9)),
      (Hour(4), Hour(10)),
      (Hour(5), Hour(11)),
      (Hour(6), Hour(12)),
      (Hour(7), Hour(1)),
      (Hour(8), Hour(2)),
      (Hour(9), Hour(3)),
      (Hour(10), Hour(4)),
      (Hour(11), Hour(5)),
      (Hour(12), Hour(6))
    )
    testHour foreach {case (hour, expectedHour) => hour.centralMirror shouldBe expectedHour}

    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    hours.foreach(h => h.centralMirror.centralMirror shouldBe h)
  }

  it should "correctly mirror horizontally" in {
    val testHour: List[(Hour, Hour)] = List(
      (Hour(1), Hour(5)),
      (Hour(2), Hour(4)),
      (Hour(3), Hour(3)),
      (Hour(4), Hour(2)),
      (Hour(5), Hour(1)),
      (Hour(6), Hour(12)),
      (Hour(7), Hour(11)),
      (Hour(8), Hour(10)),
      (Hour(9), Hour(9)),
      (Hour(10), Hour(8)),
      (Hour(11), Hour(7)),
      (Hour(12), Hour(6))
    )
    testHour foreach {case (hour, expectedHour) => hour.horizontalMirror shouldBe expectedHour}

    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    hours.foreach(h => h.horizontalMirror.horizontalMirror shouldBe h)
  }

  it should "correctly mirror vertically" in {
    val testHour: List[(Hour, Hour)] = List(
      (Hour(1), Hour(11)),
      (Hour(2), Hour(10)),
      (Hour(3), Hour(9)),
      (Hour(4), Hour(8)),
      (Hour(5), Hour(7)),
      (Hour(6), Hour(6)),
      (Hour(7), Hour(5)),
      (Hour(8), Hour(4)),
      (Hour(9), Hour(3)),
      (Hour(10), Hour(2)),
      (Hour(11), Hour(1)),
      (Hour(12), Hour(12))
    )
    testHour foreach {case (hour, expectedHour) => hour.verticalMirror shouldBe expectedHour}

    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    hours.foreach(h => h.verticalMirror.verticalMirror shouldBe h)
  }

  "Minute" should "correctly construct" in {
    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    minutes.foreach(m => 0.to(59).contains(m.toInt) shouldBe true)
  }

  it should "have toInt method" in {
    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    minutes.foreach(m => Minute(m.toInt) shouldBe m)
  }

  it should "have period 60" in {
    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    minutes.foreach(m => m.add(60) shouldBe m)
  }

  it should "have next method" in {
    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    minutes zip minutes.tail foreach {case (m1, m2) => m1.next shouldBe m2}
  }

  it should "correctly mirror centrally" in {
    val testMinute: List[(Minute, Minute)] = List(
      (Minute(0), Minute(30)),
      (Minute(5), Minute(35)),
      (Minute(10), Minute(40)),
      (Minute(15), Minute(45)),
      (Minute(20), Minute(50)),
      (Minute(25), Minute(55)),
      (Minute(30), Minute(0)),
      (Minute(35), Minute(5)),
      (Minute(40), Minute(10)),
      (Minute(45), Minute(15)),
      (Minute(50), Minute(20)),
      (Minute(55), Minute(25)),
      (Minute(59), Minute(29)),
      (Minute(60), Minute(30))
    )
    testMinute foreach {case (minute, expectedMinute) => minute.centralMirror shouldBe expectedMinute}

    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    minutes.foreach(m => m.centralMirror.centralMirror shouldBe m)
  }

  it should "correctly mirror horizontally" in {
    val testMinute: List[(Minute, Minute)] = List(
      (Minute(0), Minute(30)),
      (Minute(5), Minute(25)),
      (Minute(10), Minute(20)),
      (Minute(15), Minute(15)),
      (Minute(20), Minute(10)),
      (Minute(25), Minute(5)),
      (Minute(30), Minute(0)),
      (Minute(35), Minute(55)),
      (Minute(40), Minute(50)),
      (Minute(45), Minute(45)),
      (Minute(50), Minute(40)),
      (Minute(55), Minute(35)),
      (Minute(59), Minute(31)),
      (Minute(60), Minute(30))
    )
    testMinute foreach {case (minute, expectedMinute) => minute.horizontalMirror shouldBe expectedMinute}

    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    minutes.foreach(m => m.horizontalMirror.horizontalMirror shouldBe m)
  }

  it should "correctly mirror vertically" in {
    val testMinute: List[(Minute, Minute)] = List(
      (Minute(0), Minute(0)),
      (Minute(5), Minute(55)),
      (Minute(10), Minute(50)),
      (Minute(15), Minute(45)),
      (Minute(20), Minute(40)),
      (Minute(25), Minute(35)),
      (Minute(30), Minute(30)),
      (Minute(35), Minute(25)),
      (Minute(40), Minute(20)),
      (Minute(45), Minute(15)),
      (Minute(50), Minute(10)),
      (Minute(55), Minute(5)),
      (Minute(59), Minute(1)),
      (Minute(60), Minute(0))
    )
    testMinute foreach {case (minute, expectedMinute) => minute.verticalMirror shouldBe expectedMinute}

    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    minutes.foreach(m => m.verticalMirror.verticalMirror shouldBe m)
  }

  "Clock" should "correctly construct" in {
    val clocks: List[Clock] = (-1000).to(1000).toList.map(Clock)
    clocks.foreach(c => 0.to(11 * 60 + 59).contains(c.toInt) shouldBe true)
  }

  it should "have toInt method" in {
    val clocks: List[Clock] = (-1000).to(1000).toList.map(Clock)
    clocks.foreach(c => Clock(c.toInt) shouldBe c)
  }

  it should "have period 720" in {
    val clocks: List[Clock] = (-1000).to(1000).toList.map(Clock)
    clocks.foreach(c => c.add(720) shouldBe c)
  }

  it should "have next method" in {
    val clocks: List[Clock] = (-1000).to(1000).toList.map(Clock)
    clocks zip clocks.tail foreach {case (c1, c2) => c1.next shouldBe c2}
  }

  it should "have fromHourMinute method" in {
    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    val clocks: List[(Clock, Hour, Minute)] = for {
      hour <- hours
      minute <- minutes
    } yield (Clock(0).fromHourMinute(hour, minute), hour, minute)

    clocks foreach {case (c, h, m) => c shouldBe Clock(h.toInt * 60 + m.toInt)}
  }

  it should "have toHourMinute method" in {
    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    val clocks: List[(Clock, Hour, Minute)] = for {
      hour <- hours
      minute <- minutes
    } yield (Clock(0).fromHourMinute(hour, minute), hour, minute)

    clocks foreach {case (c, h, m) => c.toHourMinute shouldBe (h, m)}
  }

  it should "have equality combining fromHourMinute and toHourMinute methods" in {
    val hours: List[Hour] = (-24).to(24).toList.map(Hour)
    val minutes: List[Minute] = (-60).to(60).toList.map(Minute)
    val clocks: List[(Clock, Hour, Minute)] = for {
      hour <- hours
      minute <- minutes
    } yield (Clock(0).fromHourMinute(hour, minute), hour, minute)

    clocks foreach {case (c, h, m) => c.fromHourMinute(h, m).toHourMinute shouldBe (h, m)}
    clocks foreach {case (c, _, _) => c.fromHourMinute(c.toHourMinute._1, c.toHourMinute._2) shouldBe c}
  }

  it should "correctly mirror centrally" in {
    val testClock: List[(Clock, Clock)] = List(
      (Clock(0 * 60),       Clock(0).fromHourMinute(Hour(0).centralMirror, Minute(0).centralMirror)),
      (Clock(0 * 60 + 10),  Clock(0).fromHourMinute(Hour(0).centralMirror, Minute(10).centralMirror)),
      (Clock(1 * 60),       Clock(0).fromHourMinute(Hour(1).centralMirror, Minute(0).centralMirror)),
      (Clock(1 * 60 + 10),  Clock(0).fromHourMinute(Hour(1).centralMirror, Minute(10).centralMirror)),
      (Clock(2 * 60),       Clock(0).fromHourMinute(Hour(2).centralMirror, Minute(0).centralMirror)),
      (Clock(2 * 60 + 10),  Clock(0).fromHourMinute(Hour(2).centralMirror, Minute(10).centralMirror)),
      (Clock(3 * 60),       Clock(0).fromHourMinute(Hour(3).centralMirror, Minute(0).centralMirror)),
      (Clock(3 * 60 + 10),  Clock(0).fromHourMinute(Hour(3).centralMirror, Minute(10).centralMirror)),
      (Clock(4 * 60),       Clock(0).fromHourMinute(Hour(4).centralMirror, Minute(0).centralMirror)),
      (Clock(4 * 60 + 10),  Clock(0).fromHourMinute(Hour(4).centralMirror, Minute(10).centralMirror)),
      (Clock(5 * 60),       Clock(0).fromHourMinute(Hour(5).centralMirror, Minute(0).centralMirror)),
      (Clock(5 * 60 + 10),  Clock(0).fromHourMinute(Hour(5).centralMirror, Minute(10).centralMirror)),
      (Clock(6 * 60),       Clock(0).fromHourMinute(Hour(6).centralMirror, Minute(0).centralMirror)),
      (Clock(6 * 60 + 10),  Clock(0).fromHourMinute(Hour(6).centralMirror, Minute(10).centralMirror)),
      (Clock(7 * 60),       Clock(0).fromHourMinute(Hour(7).centralMirror, Minute(0).centralMirror)),
      (Clock(7 * 60 + 10),  Clock(0).fromHourMinute(Hour(7).centralMirror, Minute(10).centralMirror)),
      (Clock(8 * 60),       Clock(0).fromHourMinute(Hour(8).centralMirror, Minute(0).centralMirror)),
      (Clock(8 * 60 + 10),  Clock(0).fromHourMinute(Hour(8).centralMirror, Minute(10).centralMirror)),
      (Clock(9 * 60),       Clock(0).fromHourMinute(Hour(9).centralMirror, Minute(0).centralMirror)),
      (Clock(9 * 60 + 10),  Clock(0).fromHourMinute(Hour(9).centralMirror, Minute(10).centralMirror)),
      (Clock(10 * 60),      Clock(0).fromHourMinute(Hour(10).centralMirror, Minute(0).centralMirror)),
      (Clock(10 * 60 + 10), Clock(0).fromHourMinute(Hour(10).centralMirror, Minute(10).centralMirror)),
      (Clock(11 * 60),      Clock(0).fromHourMinute(Hour(11).centralMirror, Minute(0).centralMirror)),
      (Clock(11 * 60 + 10), Clock(0).fromHourMinute(Hour(11).centralMirror, Minute(10).centralMirror)),
      (Clock(12 * 60),      Clock(0).fromHourMinute(Hour(12).centralMirror, Minute(0).centralMirror)),
      (Clock(12 * 60 + 10), Clock(0).fromHourMinute(Hour(12).centralMirror, Minute(10).centralMirror))
    )
    testClock foreach {case (clock, expectedClock) => clock.centralMirror shouldBe expectedClock}

    val clocks: List[Clock] = (-1000).to(1000).toList.map(Clock)
    clocks.foreach(c => c.centralMirror.centralMirror shouldBe c)
  }

  it should "correctly mirror horizontally" in {
    val testClock: List[(Clock, Clock)] = List(
      (Clock(0 * 60),       Clock(0).fromHourMinute(Hour(0).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(0 * 60 + 10),  Clock(0).fromHourMinute(Hour(0).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(1 * 60),       Clock(0).fromHourMinute(Hour(1).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(1 * 60 + 10),  Clock(0).fromHourMinute(Hour(1).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(2 * 60),       Clock(0).fromHourMinute(Hour(2).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(2 * 60 + 10),  Clock(0).fromHourMinute(Hour(2).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(3 * 60),       Clock(0).fromHourMinute(Hour(3).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(3 * 60 + 10),  Clock(0).fromHourMinute(Hour(3).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(4 * 60),       Clock(0).fromHourMinute(Hour(4).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(4 * 60 + 10),  Clock(0).fromHourMinute(Hour(4).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(5 * 60),       Clock(0).fromHourMinute(Hour(5).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(5 * 60 + 10),  Clock(0).fromHourMinute(Hour(5).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(6 * 60),       Clock(0).fromHourMinute(Hour(6).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(6 * 60 + 10),  Clock(0).fromHourMinute(Hour(6).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(7 * 60),       Clock(0).fromHourMinute(Hour(7).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(7 * 60 + 10),  Clock(0).fromHourMinute(Hour(7).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(8 * 60),       Clock(0).fromHourMinute(Hour(8).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(8 * 60 + 10),  Clock(0).fromHourMinute(Hour(8).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(9 * 60),       Clock(0).fromHourMinute(Hour(9).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(9 * 60 + 10),  Clock(0).fromHourMinute(Hour(9).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(10 * 60),      Clock(0).fromHourMinute(Hour(10).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(10 * 60 + 10), Clock(0).fromHourMinute(Hour(10).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(11 * 60),      Clock(0).fromHourMinute(Hour(11).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(11 * 60 + 10), Clock(0).fromHourMinute(Hour(11).horizontalMirror, Minute(10).horizontalMirror)),
      (Clock(12 * 60),      Clock(0).fromHourMinute(Hour(12).horizontalMirror, Minute(0).horizontalMirror)),
      (Clock(12 * 60 + 10), Clock(0).fromHourMinute(Hour(12).horizontalMirror, Minute(10).horizontalMirror))
    )
    testClock foreach {case (clock, expectedClock) => clock.horizontalMirror shouldBe expectedClock}

    val clocks: List[Clock] = (-1000).to(1000).toList.map(Clock)
    clocks.foreach(c => c.horizontalMirror.horizontalMirror shouldBe c)
  }

  it should "correctly mirror vertically" in {
    val testClock: List[(Clock, Clock)] = List(
      (Clock(0 * 60),       Clock(0).fromHourMinute(Hour(0).verticalMirror, Minute(0).verticalMirror)),
      (Clock(0 * 60 + 10),  Clock(0).fromHourMinute(Hour(0).verticalMirror, Minute(10).verticalMirror)),
      (Clock(1 * 60),       Clock(0).fromHourMinute(Hour(1).verticalMirror, Minute(0).verticalMirror)),
      (Clock(1 * 60 + 10),  Clock(0).fromHourMinute(Hour(1).verticalMirror, Minute(10).verticalMirror)),
      (Clock(2 * 60),       Clock(0).fromHourMinute(Hour(2).verticalMirror, Minute(0).verticalMirror)),
      (Clock(2 * 60 + 10),  Clock(0).fromHourMinute(Hour(2).verticalMirror, Minute(10).verticalMirror)),
      (Clock(3 * 60),       Clock(0).fromHourMinute(Hour(3).verticalMirror, Minute(0).verticalMirror)),
      (Clock(3 * 60 + 10),  Clock(0).fromHourMinute(Hour(3).verticalMirror, Minute(10).verticalMirror)),
      (Clock(4 * 60),       Clock(0).fromHourMinute(Hour(4).verticalMirror, Minute(0).verticalMirror)),
      (Clock(4 * 60 + 10),  Clock(0).fromHourMinute(Hour(4).verticalMirror, Minute(10).verticalMirror)),
      (Clock(5 * 60),       Clock(0).fromHourMinute(Hour(5).verticalMirror, Minute(0).verticalMirror)),
      (Clock(5 * 60 + 10),  Clock(0).fromHourMinute(Hour(5).verticalMirror, Minute(10).verticalMirror)),
      (Clock(6 * 60),       Clock(0).fromHourMinute(Hour(6).verticalMirror, Minute(0).verticalMirror)),
      (Clock(6 * 60 + 10),  Clock(0).fromHourMinute(Hour(6).verticalMirror, Minute(10).verticalMirror)),
      (Clock(7 * 60),       Clock(0).fromHourMinute(Hour(7).verticalMirror, Minute(0).verticalMirror)),
      (Clock(7 * 60 + 10),  Clock(0).fromHourMinute(Hour(7).verticalMirror, Minute(10).verticalMirror)),
      (Clock(8 * 60),       Clock(0).fromHourMinute(Hour(8).verticalMirror, Minute(0).verticalMirror)),
      (Clock(8 * 60 + 10),  Clock(0).fromHourMinute(Hour(8).verticalMirror, Minute(10).verticalMirror)),
      (Clock(9 * 60),       Clock(0).fromHourMinute(Hour(9).verticalMirror, Minute(0).verticalMirror)),
      (Clock(9 * 60 + 10),  Clock(0).fromHourMinute(Hour(9).verticalMirror, Minute(10).verticalMirror)),
      (Clock(10 * 60),      Clock(0).fromHourMinute(Hour(10).verticalMirror, Minute(0).verticalMirror)),
      (Clock(10 * 60 + 10), Clock(0).fromHourMinute(Hour(10).verticalMirror, Minute(10).verticalMirror)),
      (Clock(11 * 60),      Clock(0).fromHourMinute(Hour(11).verticalMirror, Minute(0).verticalMirror)),
      (Clock(11 * 60 + 10), Clock(0).fromHourMinute(Hour(11).verticalMirror, Minute(10).verticalMirror)),
      (Clock(12 * 60),      Clock(0).fromHourMinute(Hour(12).verticalMirror, Minute(0).verticalMirror)),
      (Clock(12 * 60 + 10), Clock(0).fromHourMinute(Hour(12).verticalMirror, Minute(10).verticalMirror))
    )
    testClock foreach {case (clock, expectedClock) => clock.verticalMirror shouldBe expectedClock}

    val clocks: List[Clock] = (-1000).to(1000).toList.map(Clock)
    clocks.foreach(c => c.verticalMirror.verticalMirror shouldBe c)
  }
}
