/**
  * Created by Michel Edkrantz on 2015-12-11.
  */

object Day3 extends App {
  val line = scala.io.Source.fromFile("data/2015/day3.txt").mkString

  println(line)

  def charToDirection(char: Char) = char match {
    case '>' => (1, 0)
    case 'v' => (0, -1)
    case '<' => (-1, 0)
    case '^' => (0, 1)
  }

  def stringToPositions(s: String) = s.scanLeft((0,0)) { case((a: Int, b: Int), next: Char) =>
    val direction = charToDirection(next)
    (a+direction._1, b+direction._2)
  }

  val positions = stringToPositions(line)

  val unique = positions.distinct.size

  println(unique)

  val unique2 = line.grouped(2).toList.transpose.map(_.mkString).flatMap(stringToPositions).distinct.size

  println(unique2)

}
