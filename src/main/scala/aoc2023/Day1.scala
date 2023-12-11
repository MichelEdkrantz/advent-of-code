package aoc2023

import scala.util.Try

object Day1 extends App with tools.AocDay {
  val (year, day)  = (2023, 1)

  def sumLine(nums: (Int, Int)) = nums._1 * 10 + nums._2

  def countLine1(line: String): (Int, Int) = {
    val first = line.iterator.find(_.isDigit).map(_.asDigit).get
    val last = line.reverseIterator.find(_.isDigit).map(_.asDigit).get
    (first, last)
  }

  // another version to try here would be some kind of sliding window of size 5
  // regex is super fast but the reverse search is a bit ugly
  val numbers = "one|two|three|four|five|six|seven|eight|nine"
  val digitMatcher = s"($numbers|\\d)".r
  val revDigitMatcher = s"(${numbers.reverse}|\\d)".r

  def intval(string: String) = string match {
    case a if a.length == 1 && Try(a.toInt).isSuccess => a.toInt
    case "zero" => 0
    case "one" => 1
    case "two" => 2
    case "three" => 3
    case "four" => 4
    case "five" => 5
    case "six" => 6
    case "seven" => 7
    case "eight" => 8
    case "nine" => 9
  }

  def countLine2(line: String): (Int, Int) = {
    val first = digitMatcher.findFirstIn(line).map(intval).get
    val last = revDigitMatcher.findFirstIn(line.reverse).map(m => intval(m.reverse)).get
    (first, last)
  }

  val ans1 = instructions.map(countLine1).map(sumLine).sum
  println(ans1)
  assert(countLine2("twone") == (2, 1))

  val ans2Test = test2Instructions.map(countLine2).map(sumLine).sum
  println(ans2Test)
  assert(ans2Test == 281)

  val ans2 = instructions.map(countLine2).map(sumLine).sum
  println(ans2)


}
