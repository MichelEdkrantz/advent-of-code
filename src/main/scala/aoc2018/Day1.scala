package aoc2018

import scala.collection.mutable

object Day1 extends App {
  val instructions = io.Source.fromFile("data/2018/day1.txt").getLines().map(_.toInt).toSeq
  val answer1 = instructions.sum

  println(answer1)

  val frequenciesForever = Stream.continually(instructions).flatten

  val frequencies = frequenciesForever.scanLeft(0) { (cur, inc) =>
    cur + inc
  }

  val seen = mutable.Set[Int]()
  val answer2 = frequencies.filter { f =>
    !seen.add(f)
  }.take(1).toList.head

  println(answer2)
}
