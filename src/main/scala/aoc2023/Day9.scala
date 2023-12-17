package aoc2023

import tools.AocDay

object Day9 extends App with AocDay {
  val (year, day) = (2023, 9)

  def parse(instructions: Seq[String]): Seq[Vector[Int]] =
    instructions.map(_.split(" ").map(_.toInt).toVector).toVector

  def layerDiff(in: List[Vector[Int]]): List[Vector[Int]] = {
    val diff = in.last.sliding(2).map(x => x(1) - x(0)).toVector
    val allZero = diff.forall(_ == 0)
    if (allZero) {
      in
    } else {
      layerDiff(in ::: List(diff))
    }
  }

  def solveOneA(line: Vector[Int]): Int = {
    layerDiff(List(line)).map(_.last).sum
  }

  def solveOneB(line: Vector[Int]): Int = {
    layerDiff(List(line)).map(_.head).reduceRight((a, b) => a - b)
  }

  def solveProblemA(input: Seq[Vector[Int]]): Int = {
    input.map(solveOneA).sum
  }

  def solveProblemB(input: Seq[Vector[Int]]): Int = {
    input.map(solveOneB).sum
  }

  val testInput = parse(testInstructions)
  println(solveProblemA(testInput))
  println(solveProblemB(testInput))

  val input = parse(instructions)
  println(solveProblemA(input))
  println(solveProblemB(input))


}
