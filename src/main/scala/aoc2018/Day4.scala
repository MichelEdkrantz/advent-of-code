package aoc2018

import java.time.LocalDateTime

object Day4 extends App {
  val instructions = io.Source.fromFile("data/2018/day4.txt").getLines().toList.sorted

  //instructions.foreach(println)

  val logPattern = "\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)\\] (.*)".r
  val beginShiftPattern = "Guard #(\\d+) begins shift".r

  case class LogEvent(d: LocalDateTime, awake: Boolean, guard: Int)

  var currentGuard = 0
  val logEvents = instructions.map { inst =>
    val logEvent = inst match {
      case logPattern(year, month, day, hh, mm, more) =>
        val (awake, g) = more match {
          case beginShiftPattern(guard) => (true, guard.toInt)
          case "falls asleep" => (false, currentGuard)
          case "wakes up" => (true, currentGuard)
        }
        val d = LocalDateTime.of(year.toInt, month.toInt, day.toInt, hh.toInt, mm.toInt)
        LogEvent(d, awake, g)
    }
    currentGuard = logEvent.guard
    logEvent
  }

  val sleepingTimesPerGuard = logEvents.groupBy(_.guard).map { case (guard, groupEvents) =>
    val sleepingTimes = groupEvents.sliding(2).map { twoEvents =>
      (twoEvents(0), twoEvents(1))
    }.flatMap { case (event1, event2) =>
      val isSleeping = !event1.awake && event2.awake
      if (isSleeping) {
        Some(event1.d.getMinute until event2.d.getMinute)
      } else None
    }
    guard -> sleepingTimes.toList
  }

  val totalSleepingTimePerGuard = sleepingTimesPerGuard.mapValues(s => s.map(_.size).sum)
  val (g, slept) = totalSleepingTimePerGuard.maxBy(_._2)
  println(s"Guard $g sleep total $slept minutes")

  //group per midnight minute
  val minuteFrequency = sleepingTimesPerGuard(g).flatMap(_.toList).groupBy(a => a).mapValues(_.size)
  val maxMinute = minuteFrequency.maxBy(_._2)
  val answer1 = maxMinute._1 * g
  println(answer1)

  val maxMinutePerGuard: Map[Int, (Int, Int)] = sleepingTimesPerGuard.view.mapValues { ranges =>
    val v = ranges.flatMap(_.toList).groupBy(a => a).view.mapValues(_.size).toMap
    if(v.nonEmpty) v.maxBy(_._2) else (0,0)
  }.toMap
  val max = maxMinutePerGuard.maxBy(_._2._2)
  println(max)

  val answer2 = max._1 * max._2._1
  println(answer2)

}
