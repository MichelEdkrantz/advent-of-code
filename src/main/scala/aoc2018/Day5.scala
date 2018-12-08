package aoc2018

object Day5 extends App {

  val instructions = io.Source.fromFile("data/2018/day5.txt").mkString

  def react(str: String) = str.foldLeft(List.empty[Char]) {
    case (head :: tail, next) if (head ^ next) == 32 => tail
    case (seq, next) => next :: seq
  }

  val reduced = react(instructions)
  println(reduced.reverse.mkString)
  val answer1 = reduced.length
  println(answer1)

  val redByLetter = ('a' to 'z').map { c =>
    c -> instructions.filter(ch => ch != c - 32 && ch != c)
  }.toMap.mapValues(react)
  val min = redByLetter.mapValues(_.size).minBy(_._2)
  println(min._1, min._2)

}
