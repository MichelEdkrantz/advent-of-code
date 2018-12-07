package aoc2016

/**
  * Created by Michel Edkrantz on 2016-12-03.
  */
object Day3 extends App {
  val instructions = io.Source.fromFile("data/2016/day3.txt").getLines().toList
  type TriangleDef = (Int, Int, Int)
  val lines = instructions.map(_.trim).map { line =>
    line.split("\\s+").map(_.toInt)
  }
  val triangles1 = lines.map(t => (t(0), t(1), t(2)))
  val triangles2 = lines.transpose.flatMap(_.grouped(3).map(t => (t(0), t(1), t(2))))

  def isValid(t: TriangleDef): Boolean = {
    t._1 + t._2 > t._3 && t._1 + t._3 > t._2 && t._2 + t._3 > t._1
  }
  print(triangles1.count(isValid))
  print(triangles2.count(isValid))

}
