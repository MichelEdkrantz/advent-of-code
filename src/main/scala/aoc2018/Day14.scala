package aoc2018

import scala.collection.mutable.ArrayBuffer

object Day14 extends App {
  val input = "084601"

  def run() = {
    val recipies = ArrayBuffer[Int](3, 7)
    var elves = Vector(0, 1)
    Iterator.from(0).map { case(t) =>
      if(t % 1000000 == 0)
        println(s"t=$t")
      //println(s"${recipies.mkString(" ")}")
      val ints = elves.map(recipies).sum.toString.map(_.toInt - 48)
      recipies.appendAll(ints)
      elves = elves.map(e => (e + 1 + recipies(e)) % recipies.length)
      recipies
    }
  }

  def part1(n: Int): String = run().find(_.length >= n + 10).head.takeRight(10).mkString
  def part2(seq: String) = {
    val s = seq.map(_.toInt - 48)
    val n = seq.length
    run().map {
      case x if x.takeRight(n) == s => x.length - n
      case x if x.takeRight(n+1).dropRight(1) == s => x.length - n - 1
      case _ => 0
    }.filter(_ > 0).next()
  }

  println(part1(9), "5158916779")
//  println(part1(5), "0124515891")
//  println(part1(18), "9251071085")
//  println(part1(6), "9251071085")
 println(part1(input.toInt))
 // println(part2("59414"), 2018)
 // println(part2("92510"), 18)
  println(part2(input))
  //println(run("37", 2018), "5941429882")

}
