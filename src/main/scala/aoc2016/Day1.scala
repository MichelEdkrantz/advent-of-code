package aoc2016

/**
  * Created by Michel Edkrantz on 2016-12-03.
  */
object Day1 extends App {
  //val instructions = "R8, R4, R4, R8"
  val instructions = io.Source.fromFile("data/2016/day1.txt").mkString
  val steps = instructions.split(", ").toVector
  val visitationState = steps.scanLeft(((0,0),1)) {
    case (((x,y),d), instruction) => {
      println(s"$x, $y, $d, next: ${instruction}")
      val len = instruction.tail.toInt
      val d2 = instruction.head match {
        case 'R' => Math.floorMod(d - 1, 4)
        case 'L' => Math.floorMod(d + 1, 4)
      }
      val (dx, dy) = d2 match {
        case 0 => (len, 0)
        case 1 => (0, len)
        case 2 => (-len, 0)
        case 3 => (0, -len)
      }
      ((x + dx, y + dy), d2)
    }
  }
  println(visitationState.last)
  println(dist(visitationState.last._1))

  //part zwei
  var seen = Set[(Int, Int)]()
  def dist(pos: (Int, Int)) = Math.abs(pos._1) + Math.abs(pos._2)

  //create iterator for below.
  def range(from: Int, to: Int) =
    if(from == to) Seq(from)
    else from.until(to).by(if(from < to) 1 else -1)

  visitationState.map(_._1).toList.sliding(2).foreach { pair =>
      for {
        x <- range(pair.head._1, pair.last._1)
        y <- range(pair.head._2, pair.last._2)
        p = (x,y)
      } {
        println(p)
        if(seen.contains(p)) {
          println(s"Found: $p with distance=${p._1 + p._2}")
        } else {
          seen += p
        }
      }
  }


}
