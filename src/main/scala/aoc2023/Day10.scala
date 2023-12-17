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

  def findStart(grid: CharGrid) = (for {
    (row, y) <- grid.zipWithIndex
    (cell, x) <- row.zipWithIndex
    if cell == 'S'
  } yield (x, y)).head

  def createDistanceMap(grid: CharGrid): mutable.Map[(Int, Int), Int] = {
    mutable.Map() ++ (for {
      (row, y) <- grid.zipWithIndex
      (_, x) <- row.zipWithIndex
    } yield ((x, y) -> Int.MaxValue)).toMap
  }

  def checkBounds(grid: CharGrid, pos: (Int, Int)): Boolean = {
    val (x, y) = pos
    x >= 0 && x < grid.head.length && y >= 0 && y < grid.length
  }

  def getNeighbors(grid: CharGrid, pos: (Int, Int)): List[(Int, Int)] = {
    val (x, y) = pos
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
        checkBounds(grid, (x, y)) && okConnectingChars.contains(grid(y)(x))
      }.map(c => (c._1, c._2))
  }

  def solveOne(grid: CharGrid) = {
    val start = findStart(grid)
    val distances = createDistanceMap(grid)
    distances(start) = 0

    // use dijkstra's algorithm
    // the nodes not on the "loop" will have a distance of Int.MaxValue,
    // so we can stop when we reach that
    val visited = mutable.Set[(Int, Int)]()
    var mDist = 0
    while (mDist < Int.MaxValue) {
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
    /*for {
      (row, y) <- grid.zipWithIndex
      (_, x) <- row.zipWithIndex
    } yield {
      val d = distances((x, y))
      if(d == Int.MaxValue) -1 else d
    }*/
    distances.toMap
  }

  def solveProblemA(grid: CharGrid) = {
    val distances = solveOne(grid)

    def getDist(pos: (Int, Int)): Int = {
      val d = distances(pos)
      if (d == Int.MaxValue) -1 else d.toChar
    }

    val d = grid.zipWithIndex.map(row => row._1.zipWithIndex.map(cell => getDist((cell._2, row._2))))
    d.foreach(row => println(row.map(c => f"$c%-3s").mkString("|")))

    val maxD = distances.filterNot(d => d._2 == Int.MaxValue).maxBy(_._2)._2
    maxD
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
