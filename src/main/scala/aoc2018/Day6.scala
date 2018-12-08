package aoc2018

object Day6 extends App {
  val instructions = io.Source.fromFile("data/2018/day6.txt").getLines()
  val points = instructions.map(_.split(", ").map(_.toInt)).map(l => (l(0), l(1))).toVector
  val indexedPoints = points.zipWithIndex

  val maxX = points.maxBy(_._1)._1
  val maxY = points.maxBy(_._2)._2

  val limit = 10000

  def abs(x: Int, y: Int) = Math.abs(x) + Math.abs(y)
  def dist(p1: (Int, Int), p2: (Int, Int)) = abs(p1._1 - p2._1, p1._2 - p2._2)

  def closest(xy: (Int, Int)): Option[(Int, Int)] = {
    val closestPoints = indexedPoints.map{ case (p,i) => (dist(p, xy), i) }.sortBy(_._1)
    if(closestPoints(0)._1 == closestPoints(1)._1) None
    else Some(closestPoints(0))
  }

  def partA()= {
    // calculate over an area that is bigger, and compare counts in the bounded vs unbounded area
    val extInd = for (x <- -1 to (maxX+1); y <- -1 to (maxY+1)) yield (x,y)
    val items = extInd.flatMap { case xy => closest(xy).map(_._2).map(p => (xy, p)) }
    val closestPerPoint = items.groupBy(_._2)
    val countPerPointExt = closestPerPoint.mapValues(_.size)
    val countPerPointBounded = closestPerPoint.mapValues(v => v.filterNot {
      case (p, i) => p._1 < 0 || p._1 > maxX || p._2 < 0 || p._2 > maxY
    }).mapValues(_.size)
    // any points that get a higher count in the bigger area can be excluded
    val unboundedPoints = countPerPointExt.filter(v => v._2 == countPerPointBounded(v._1))
    val answer1 = unboundedPoints.maxBy(_._2)
    println(answer1)
  }

  def partB(): Unit = {
    val ind = for (x <- 0 to (maxX); y <- 0 to (maxY)) yield (x,y)

    val items = ind.map { case xy =>
      points.map(p => dist(p, xy)).sum
    }.filter(_ < limit)

    val answer2 = items.size
    println(answer2)
  }

  partA()
  partB()

}
