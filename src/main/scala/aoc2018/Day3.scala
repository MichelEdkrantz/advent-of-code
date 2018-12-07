package aoc2018

object Day3 extends App {
  case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int)
  val instructions = io.Source.fromFile("data/2018/day3.txt").getLines().toList

  val pattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

  def parseClaim(str: String): Claim = {
    str match {
      case pattern(a,b,c,d,e) => Claim(a.toInt, b.toInt, c.toInt, d.toInt, e.toInt)
    }
  }

  val claims = instructions.map(parseClaim)
  val maxHeight = claims.map(c => c.y + c.h).max
  val maxWidth = claims.map(c => c.x + c.w).max
  println(s"Got canvas of size ($maxHeight, $maxWidth)") //row order
  val canvas = Array.ofDim[Int](maxHeight, maxWidth)

  def indexIterator(c: Claim) = {

  }

  claims.foreach { c =>
    for {
      i <- c.x until (c.x + c.w)
      j <- c.y until (c.y + c.h)
    } {
      canvas(j)(i) += 1
    }
  }

//  canvas.foreach { line =>
//    println(line.mkString)
//  }

  val answer1 = canvas.map(_.filter(_ >= 2).size).sum
  println(answer1)

  val nonOverlappingClaim = claims.find { c =>
    val tiles = for {
      i <- c.x until (c.x + c.w)
      j <- c.y until (c.y + c.h)
    } yield {
      canvas(j)(i) == 1
    }
    tiles.forall(t => t)
  }.head

  println(nonOverlappingClaim.id)

}
