package aoc2023

import tools.AocDay

object Day6 extends App with AocDay {
  val (year, day) = (2023, 6)
  case class TimeDistance(time: Int, distance: Int)
  def parse(lines: List[String]): Vector[TimeDistance] = {
    val times = lines(0).replace("Time:", "").split("\\s+").map(_.trim).filter(_.nonEmpty).map(_.toInt)
    val distances = lines(1).replace("Distance:", "").split("\\s+").map(_.trim).filter(_.nonEmpty).map(_.toInt)
    times.zip(distances).map(a => TimeDistance.apply(a._1, a._2)).toVector
  }

  def getPosibleDistances(time: Int) = for {
    i <- 1 to time
  } yield TimeDistance(time, ((time - i) * i))

  def getNRecordDistances(time: Int, currentRecord: Int): Int = {
    getPosibleDistances(time).count(_.distance > currentRecord)
  }

  def solveProblem1(input: Seq[TimeDistance]) = {
    input.map(td => getNRecordDistances(td.time, td.distance)).product
  }

  val testInput = parse(testInstructions)
  val input = parse(instructions)
  println(getPosibleDistances(7))
  println(getNRecordDistances(7, 9))

  println(solveProblem1(testInput))
  println(solveProblem1(input))
}
