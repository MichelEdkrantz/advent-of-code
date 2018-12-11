package aoc2018



object Day10 extends App {

  import tools.Tools.TupleAdd

  val instructions = io.Source.fromFile("data/2018/day10.txt").getLines
  val pattern = "position=<([\\d-]+),([\\d-]+)>velocity=<([\\d-]+),([\\d-]+)>".r
  val parsed = instructions.map(_.replace(" ", "")).map {
    case pattern(x, y, u, v) => ((x.toInt, y.toInt), (u.toInt, v.toInt))
  } toList
  val initialPosition = parsed.map(_._1).toVector
  val speed = parsed.map(_._2).toVector

  val states = Stream.from(0).scanLeft(initialPosition) { case (positions, t) =>
    positions.zip(speed).map(a => a._1 + a._2)
  }

  def getBounds(s: Vector[(Int, Int)]) = (
    (s.minBy(_._1)._1, s.maxBy(_._1)._1),
    (s.minBy(_._2)._2, s.maxBy(_._2)._2)
  )

  def printState(s: Vector[(Int, Int)]) = {
    val bounds = getBounds(s)
    val perLine = s.groupBy(_._2).mapValues(_.map(_._1))
    val xs = bounds._1._1 to bounds._1._2
    val ys = bounds._2._1 to bounds._2._2
    ys.foreach { y =>
      val vals = perLine.getOrElse(y, Vector()).toSet
      val l = xs.map(el => if (vals.contains(el)) "#" else ".").mkString
      println(l)
    }
  }

  val first = states.zipWithIndex.map { case (s, t) =>
    val b = getBounds(s)
    (s, b._2._2 - b._2._1, t)
  }.sliding(2).map(_.toVector)
    .find { v => v(0)._2 < v(1)._2}.head.head

  printState(first._1)
  println(first._3)

  //  states.take(10).foreach { s =>
  //    println("")
  //    printState(s)
  //  }
}
