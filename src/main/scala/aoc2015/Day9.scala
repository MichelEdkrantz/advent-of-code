/**
  * Created by Michel Edkrantz on 2015-01-05.
  */

import scala.annotation.tailrec
import scala.runtime.ScalaRunTime._
object Day9 extends App {
  type TSPPath = Seq[Int]
  type DistanceMatrix = Array[Array[Int]]
  type BestPathResolver = Seq[(TSPPath, Int)] => (TSPPath, Int)

  val lines = scala.io.Source.fromFile("data/2015/day9.txt").getLines.toSeq

  def buildDistanceMatrix(lines: Seq[String]): (Vector[String], DistanceMatrix) = {
    val pattern = """(\w+) to (\w+) = (\d+)""".r

    val distances = lines.map({
      case pattern(cityA, cityB, dist) => (cityA, cityB, dist.toInt)
    })

    val cities = distances.flatMap(d => List(d._1, d._2)).toSet.toVector

    val distanceMatrix = Array.ofDim[Int](cities.length, cities.length)
    distances.foreach {
      case(cityA, cityB, dist) =>
        val indA = cities.indexOf(cityA)
        val indB = cities.indexOf(cityB)
        distanceMatrix(indA)(indB) = dist
        distanceMatrix(indB)(indA) = dist
    }

    (cities, distanceMatrix)
  }

  val (cities, distanceMatrix) = buildDistanceMatrix(lines)

  println(cities)
  println(stringOf(distanceMatrix))



  //implement a slow TSP solver
  def solveTSP(distanceMatrix: DistanceMatrix, bestPathResolver: BestPathResolver): (TSPPath, Int) = {
    //start at the first index, and visit recursively
    def travel(path: TSPPath, len: Int): (TSPPath, Int) = {
      if (path.length == distanceMatrix.length) {
        (path, len) //we are done
      } else {
        //visit all branches not yet visited
        val unvisited = (0 until distanceMatrix.length).filterNot(path.contains(_))
        val paths = unvisited map { i =>
          travel(path :+ i, len + distanceMatrix(path.last)(i))
        }
        bestPathResolver(paths)
      }
    }

    //since we can start and end at different index, we must use every city as starting point
    val bestPaths = 0 until distanceMatrix.length map { i =>
      val path = Seq(i)
      travel(path, 0)
    }

    //return the optimal index order and the shortest path
    bestPathResolver(bestPaths)
  }

  def minResolver(paths: Seq[(TSPPath, Int)]) = paths.zipWithIndex.minBy(_._1._2)._1
  def maxResolver(paths: Seq[(TSPPath, Int)]) = paths.zipWithIndex.maxBy(_._1._2)._1

  val (minPath, min) = solveTSP(distanceMatrix, minResolver)
  println("Min", minPath, min)
  println(minPath.map(cities(_)))
  val (maxPath, max) = solveTSP(distanceMatrix, maxResolver)
  println("Max", maxPath, max)
  println(maxPath.map(cities(_)))

}
