/**
  * Created by Michel Edkrantz on 2015-01-06.
  */

import scala.annotation.tailrec

object Day10 extends App {

  val myPuzzleInput = "1113122113"


  def lookAndSaySlow(s: String): String = {

    //match 1 token, followed by 0 or more of the same and
    val pattern = """(.)(\1*)(.*)""".r

    @tailrec
    def lookAndSayHelper(part1: String, part2: String): String = part2 match {
      case "" => part1 //we are done
      case pattern(digit, repeat, rest) =>
        lookAndSayHelper(part1 + (repeat.length + 1).toString + digit, rest)
    }
    lookAndSayHelper("", s)
  }

  def lookAndSayFast(s: String): String = {
    val pattern = """((\d)(?:\2)*)""".r
    pattern.findAllMatchIn(s).toSeq.map { m =>
      m.group(1).length.toString + m.group(2)
    } mkString
  }

//  def createStream(seed: String): Stream[String] = {
//    lazy val stream: Stream[String] = Stream.cons(seed, stream.map(lookAndSay))
//    stream
//  }
//  val stream = createStream(myPuzzleInput)
//  println(stream(2).length)

  def lookAndSay = lookAndSayFast _

  assert(lookAndSay("1") == "11")
  assert(lookAndSay("11") == "21")
  assert(lookAndSay("21") == "1211")
  assert(lookAndSay("1211") == "111221")
  assert(lookAndSay("111221") == "312211")

  println(lookAndSayFast("122233"))

  var current = myPuzzleInput
  for{
    i <- 1 to 50
  } {
    current = lookAndSay(current)
    println(s"Done with i=$i, length=${current.length}")
  }

}
