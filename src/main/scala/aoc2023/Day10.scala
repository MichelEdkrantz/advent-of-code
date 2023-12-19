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

  def getNeighbors(grid: CharGrid, pos: Position): List[Position] = {
    val (x, y) = (pos.x, pos.y)
    val cell = grid(y)(x)
    val combos: List[(Int, Int, List[Char])] = cell match {
      case '|' => List((x, y - 1, north), (x, y + 1, south))
      case '-' => List((x - 1, y, west), (x + 1, y, east))
      case '7' => List((x, y + 1, south), (x - 1, y, west))
      case 'J' => List((x, y - 1, north), (x - 1, y, west))
      case 'F' => List((x, y + 1, south), (x + 1, y, east))
      case 'L' => List((x, y - 1, north), (x + 1, y, east))
      case 'S' => List((x, y - 1, north), (x, y + 1, south), (x - 1, y, west), (x + 1, y, east))
    }

    //we must now check the bounds and also check that the neighbor has a compatible cell
    combos
      .filter { case (x, y, okConnectingChars) =>
        checkBounds(grid, Position(x, y)) && okConnectingChars.contains(grid(y)(x))
      }.map(c => Position(c._1, c._2))
  }

  def solveOne(grid: CharGrid) = {
    val start = findStart(grid)
    val distances = createDistanceMap(grid)
    distances(start) = 0

    // use dijkstra's algorithm
    // the nodes not on the "loop" will have a distance of Int.MaxValue,
    // so we can stop when we reach that
    val visited = mutable.Set[Position]()
    var mDist = 0
    while (mDist < Int.MaxValue) {
      //IMPROVE this is slow on a large grid
      val (pos, minDist) = distances.filterNot(d => visited.contains(d._1)).minBy(_._2)
      //println("pos: " + pos + " minDist: " + minDist)
      mDist = minDist
      if (minDist != Int.MaxValue) {
        getNeighbors(grid, pos).foreach { neighbor =>
          val dist = minDist + 1
          if (dist < distances(neighbor)) {
            distances(neighbor) = dist
          }
        }
        visited += pos
      }
    }
    grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        val d = distances(Position(x, y))
        if(d == Int.MaxValue) -1 else d
      }
    }
  }

  def printDistanceGrid(distances: Vector[Vector[Int]]) = {
    distances.foreach(row =>
      println(row.map(c => f"$c%-3s").mkString)
    )
  }

  def findPath(start: Position,
               grid: CharGrid,
               distances: Vector[Vector[Int]]): Seq[Position] = {
    // walk in all possible directions and find the shortest path
    val seeds = getNeighbors(grid, start)
    val paths = seeds.map { seed =>
      val path = mutable.ListBuffer[Position]()
      var prev = start
      var pos = seed
      // look for a neighbors with larger distance
      while (distances(pos.y)(pos.x) >= distances(prev.y)(prev.x)) {
        path += pos
        prev = pos
        pos = getNeighbors(grid, pos).maxBy(n => distances(n.y)(n.x))
      }
      path.toList
    }
    val twoPaths = paths.groupBy(_.size).maxBy(_._1)._2
    start :: twoPaths.head ::: twoPaths.last.init.reverse
  }

  def solveProblemA(grid: CharGrid) = {
    val distances = solveOne(grid)
    val path = findPath(findStart(grid), grid, distances)
    val pathCoords = path.toSet
    distances.zipWithIndex.foreach { case (row, y) =>
      println(row.zipWithIndex.map {
        case (c, x) if c > -1 && pathCoords.contains(Position(x, y)) => "x"
        case _ => "."
      }.mkString)
    }
    //println(path)
    distances.flatten.max
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
