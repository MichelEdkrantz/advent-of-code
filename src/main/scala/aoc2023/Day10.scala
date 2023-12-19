package aoc2023

import tools.AocDay

import scala.util.Try

object Day10 extends App with AocDay {
  val (year, day) = (2023, 10)
  type CharGrid = Vector[Vector[Char]]

  def parse(instructions: List[String]): CharGrid = instructions.map(_.toVector).toVector

  case class Position(x: Int, y: Int) {
    def +(p: Position) = Position(p.x + x, p.y + y)
    def -(p: Position) = Position(x - p.x, y - p.y)
  }

  def findStart(grid: CharGrid): Position = (for {
    (row, y) <- grid.zipWithIndex
    (cell, x) <- row.zipWithIndex
    if cell == 'S'
  } yield Position(x, y)).head

  // can this be done with complex numbers or some kind of rotation?
  def next(current: Char, direction: Char): (Position, Char) = (current, direction) match {
    case ('|', 'N') => (Position(0, -1), 'N')
    case ('|', 'S') => (Position(0, 1), 'S')
    case ('-', 'E') => (Position(1, 0), 'E')
    case ('-', 'W') => (Position(-1, 0), 'W')
    case ('F', 'N') => (Position(1, 0), 'E')
    case ('F', 'W') => (Position(0, 1), 'S')
    case ('7', 'E') => (Position(0, 1), 'S')
    case ('7', 'N') => (Position(-1, 0), 'W')
    case ('J', 'S') => (Position(-1, 0), 'W')
    case ('J', 'E') => (Position(0, -1), 'N')
    case ('L', 'W') => (Position(0, -1), 'N')
    case ('L', 'S') => (Position(1, 0), 'E')
  }

  /*def checkBounds(grid: CharGrid, pos: Position): Boolean = {
    val (x, y) = (pos.x, pos.y)
    x >= 0 && x < grid.head.length && y >= 0 && y < grid.length
  }*/

  def findPath(grid: CharGrid, start: Position): List[Position] = {
    def walk(direction: Char, path: List[Position]): List[Position] = {
      val current = path.head
      val currentChar = grid(current.y)(current.x)
      if (currentChar == 'S') {
        path.tail
      } else {
        val (delta, dir) = next(currentChar, direction)
        val nextPos = Position(current.x + delta.x, current.y + delta.y)
        walk(dir, nextPos :: path)
      }
    }
    val neigbours = List(
      (start + Position(0, -1), 'N', Seq('|', 'F', '7')),
      (start + Position(0, 1), 'S', Seq('|', 'J', 'L')),
      (start + Position(-1, 0), 'W', Seq('-', 'F', 'L')),
      (start + Position(1, 0), 'E', Seq('-', '7', 'J'))
    )
    val oneNeighbour = neigbours.find { case (np, dir, okConnections) =>
      Try {
        grid(np.y)(np.x)
      }.toOption match {
        case Some(c) => okConnections.contains(c)
        case None => false
      }
    }.get
    walk(oneNeighbour._2, oneNeighbour._1 :: start :: Nil)
  }

  def solveProblemA(grid: CharGrid): Int = {
    val start = findStart(grid)
    val path = findPath(grid, start)
    path.length / 2
  }

  val testInput = parse(testInstructions)
  val test2Input = parse(test2Instructions)
  val input = parse(instructions)
  println(solveProblemA(testInput))
  //println(solveProblemA(test2Input))
  println(solveProblemA(input))
}
