package aoc2016

/**
  * Created by Michel Edkrantz on 2016-12-10.
  */
object Day6 extends App {
  val test_input = Seq(
    "eedadn", "drvtee", "eandsr", "raavrd", "atevrs", "tsrnev", "sdttsa", "rasrtv",
    "nssdts", "ntnada", "svetve", "tesnvt", "vntsnd", "vrdear", "dvrsen", "enarar"
  )

  val input = io.Source.fromFile("data/2016/day6.txt").getLines().toList.map(_.trim)

  val sorted = input.map(_.toCharArray)
    .transpose
    .map(_.groupBy(identity).mapValues(_.size).toSeq.sortBy(_._2).map(_._1))

  val code1 = sorted.map(_.last).mkString
  val code2 = sorted.map(_.head).mkString
  println(code1)
  println(code2)
}
