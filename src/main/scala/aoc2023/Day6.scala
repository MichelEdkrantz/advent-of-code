package aoc2023

import tools.AocDay

object Day6 extends App with AocDay {
  val (year, day) = (2023, 6)
  case class TimeDistance(time: Long, distance: Long)
  def parse(lines: List[String]): Vector[TimeDistance] = {
    val times = lines(0).replace("Time:", "").split("\\s+").map(_.trim).filter(_.nonEmpty).map(_.toInt)
    val distances = lines(1).replace("Distance:", "").split("\\s+").map(_.trim).filter(_.nonEmpty).map(_.toInt)
    times.zip(distances).map(a => TimeDistance.apply(a._1, a._2)).toVector
  }
  def flatten(tdSeq: Seq[TimeDistance]) = {
    val t = tdSeq.map(_.time.toString).mkString.toLong
    val d = tdSeq.map(_.distance.toString).mkString.toLong
    TimeDistance(t, d)
  }

  def getPosibleDistances(time: Long) = for {
    i <- 1L to time
  } yield TimeDistance(time, ((time - i) * i))

  def getNRecordDistances(time: Long, currentRecord: Long): Int = {
    getPosibleDistances(time).count(_.distance > currentRecord)
  }

  def solveProblem(input: Seq[TimeDistance]) = {
    input.map(td => getNRecordDistances(td.time, td.distance)).product
  }

  val testInput = parse(testInstructions)
  val input = parse(instructions)
  val input2 = flatten(input)
  println(getPosibleDistances(7))
  println(getNRecordDistances(7, 9))

  println(solveProblem(testInput))
  println(solveProblem(input))
  println(solveProblem(Seq(input2)))
}
