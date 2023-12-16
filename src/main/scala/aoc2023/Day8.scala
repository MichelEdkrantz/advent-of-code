package aoc2023

import tools.AocDay

object Day8 extends App with AocDay {
  val (year, day) = (2023, 8)

  case class ParsedFile(seq: String, rows: Seq[Row]) {
    val chars = seq.toCharArray
    val rowMap: Map[String, Row] = rows.groupMap(_.from)(identity).view.mapValues(_.head).toMap
  }

  case class Row(from: String, left: String, right: String)

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def parse(instructions: Seq[String]): ParsedFile = {
    val rows = instructions.drop(2).map {
      case s"$pos = ($left, $right)" => Row(pos, left, right)
    }
    ParsedFile(instructions.head, rows)
  }

  def solveOne(file: ParsedFile, seed: Row, pred: Row => Boolean): Int = {
    Iterator.continually(file.chars)
      .flatten
      .scanLeft(seed) { case (row, char) =>
        val next = if (char == 'L') row.left else row.right
        file.rowMap(next)
      }
      .takeWhile(pred)
      .length
  }

  def solveProblemA(file: ParsedFile) = {
    val first = file.rowMap("AAA")
    solveOne(file, first, _.from != "ZZZ")
  }

  def solveProblemB(file: ParsedFile) = {
    val it1 = Iterator.continually(file.chars).flatten
    val startRows = file.rows.filter(_.from.endsWith("A"))
    val it2 = it1.scanLeft(startRows) { case (rows, char) =>
      rows.map { row =>
        assert(!(row.from == row.right && row.from == row.left))
        val next = if (char == 'L') row.left else row.right
        file.rowMap(next)
      }
    }
    val a = it2.takeWhile(rows => !rows.forall(_.from.endsWith("Z")))
    a.length
  }

  def solveProblemB2(file: ParsedFile) = {
    val startRows = file.rows.filter(_.from.endsWith("A"))
    val lengths = startRows.map(solveOne(file, _, !_.from.endsWith("Z")))
    //calc lcm of these numbers
    lengths.map(_.toLong).toSet.reduce(lcm)
  }

  val testFile = parse(testInstructions)
  println(solveProblemA(testFile))

  val testFile2 = parse(test2Instructions)
  println(solveProblemA(testFile2))

  val testFile3 = parse(test3Instructions)
  println(solveProblemB2(testFile3))

  val file = parse(instructions)
  println(solveProblemA(file))

  println(solveProblemB2(file))
}
