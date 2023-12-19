package aoc2023

import tools.AocDay

import scala.collection.mutable

object Day10 extends App with AocDay {
  val (year, day) = (2023, 10)
  type CharGrid = Vector[Vector[Char]]

  def parse(instructions: List[String]): CharGrid = instructions.map(_.toVector).toVector

  val north = '|' :: '7' :: 'F' :: Nil
  val south = '|' :: 'J' :: 'L' :: Nil
  val east = '-' :: '7' :: 'J' :: Nil
  val west = '-' :: 'L' :: 'F' :: Nil

  case class Position(x: Int, y: Int)

  def findStart(grid: CharGrid): Position = (for {
    (row, y) <- grid.zipWithIndex
    (cell, x) <- row.zipWithIndex
    if cell == 'S'
  } yield Position(x, y)).head

  def createDistanceMap(grid: CharGrid): mutable.Map[Position, Int] = {
    mutable.Map() ++ (for {
      (row, y) <- grid.zipWithIndex
      (_, x) <- row.zipWithIndex
    } yield (Position(x, y) -> Int.MaxValue)).toMap
  }

  def checkBounds(grid: CharGrid, pos: Position): Boolean = {
    val (x, y) = (pos.x, pos.y)
    x >= 0 && x < grid.head.length && y >= 0 && y < grid.length
  }

  // TODO, this code is flawed. Using djiikstra's algorithm for part 1 is overkill, since we only need to follow the path
  // it never forks, so we can just follow the path and keep track of the distance
  // for part b, getting the path will be for free.

  def solveProblemA(grid: CharGrid): Int = {

  }

  val testInput = parse(testInstructions)
  val test2Input = parse(test2Instructions)
  val test3Input = parse(test3Instructions)
  val input = parse(instructions)
  println(solveProblemA(testInput))
  println(solveProblemA(test2Input))
  println(solveProblemA(test3Input))
  println(solveProblemA(input))
}
