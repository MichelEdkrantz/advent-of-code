/**
  * Created by Michel Edkrantz on 2015-01-06.
  */

/*
import aoc2015.JsonUtil
import aoc2015.JsonUtil.Dict

import scala.io.Source
object Day12 extends App {

  val jsonString = Source.fromFile("data/2015/day12.json").getLines().mkString

  val digitPattern = """(-?\d+)""".r

  val sum = digitPattern.findAllIn(jsonString).toSeq.map(_.toInt).sum

  println(sum)

  //part 2, must actually unpack json
  val input = JsonUtil.fromJson[List[Any]](jsonString)

  def sumDeep(input: Any): Int = input match {
    case seq: Seq[Any] => seq.map(sumDeep).sum
    case d: Dict => {
      if (d.values.exists(_ == "red") ) {
        0
      } else {
        d.map(a => sumDeep(a._2)).sum
      }
    }
    case int: Int => int
    case default => 0
  }

  println(sumDeep(input))

}
 */

