package aoc2016

import scala.annotation.tailrec

/**
  * Created by Michel Edkrantz on 2016-12-11.
  */
object Day9 extends App {

  val startsWithLetters = "(\\w+)(.*)".r
  val multiplyPattern = """\((\d+)x(\d+)\)(.*)""".r

  def decompress1(s: String) = {
    @tailrec
    def _decompress(done: Int, rest: String): Int = rest match {
      case rest if rest.length == 0 => done
      case startsWithLetters(letters, rest) =>
        _decompress(done + letters.length, rest)
      case multiplyPattern(a,b,rest) => {
        val x = rest.take(a.toInt).mkString * b.toInt
        _decompress(done + x.length,rest.drop(a.toInt))
      }
    }
    _decompress(0, s)
  }

  def decompress2(s: String) = {
    @tailrec
    def _decompress(done: Int, rest: String): Int = rest match {
      case rest if rest.length == 0 => done
      case startsWithLetters(letters, rest) =>
        _decompress(done + letters.length, rest)
      case multiplyPattern(a,b,rest) => {
        val x = rest.take(a.toInt).mkString * b.toInt
        _decompress(done, x + rest.drop(a.toInt))
      }
    }
    _decompress(0, s)
  }

//  def decompress3(s: String) = {
//    @tailrec
//    def _decompress(done: Int, rest: String): (Int, Int) = rest match {
//      case rest if rest.length == 0 => done
//      case startsWithLetters(letters, rest) =>
//        _decompress(done + letters.length, rest)
//      case multiplyPattern(a,b,rest) => {
//        val x = rest.take(a.toInt).mkString * b.toInt
//        _decompress(done, x + rest.drop(a.toInt))
//      }
//    }
//    _decompress(0, s)
//  }

  def decomp(s: String) = {

  }

  val tests = Map(
    "A(1x5)BC" -> "ABBBBBC",
    "(3x3)XYZ" -> "XYZXYZXYZ",
    "A(2x2)BCD(2x2)EFG" -> "ABCBCDEFEFG",
    "X(8x2)(3x3)ABCY" -> "X(3x3)ABC(3x3)ABCY"
  )

  val testLong = Map(
    "(3x3)XYZ" -> "XYZXYZXYZ".length,
    "X(8x2)(3x3)ABCY" -> "XABCABCABCABCABCABCY".length,
    "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" -> 445,
    "(27x12)(20x12)(13x14)(7x10)(1x12)A" -> 241920
  )

  testLong.foreach { case(key, value) =>
    decompress2(key) match {
      case d if d == value =>
        println(s"Decompressed $key to $d")
      case d =>
        println(s"Decompressed $key to $d, should be $value.")
    }
  }
  //val input = io.Source.fromFile("data/2016/day9.txt").mkString
  //println(decompress(input))
}
