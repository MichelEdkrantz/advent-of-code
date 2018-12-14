package aoc2018

import scala.collection.mutable.ArrayBuffer

object Day14 extends App {
  val input = "084601"

  //create an iterator of all new recipes to be invented
  def run() = {
    val recipies = ArrayBuffer[Int](3, 7)
    var elves = Vector(0, 1)
    val it = Iterator.from(0).flatMap { case(t) =>
      if(t % 1000000 == 0)
        println(s"t=$t")
      //println(s"${recipies.mkString(" ")}")
      val ints = elves.map(recipies).sum.toString.map(_.toInt - 48)
      recipies.appendAll(ints)
      elves = elves.map(e => (e + 1 + recipies(e)) % recipies.length)
      ints
    }
    recipies.iterator ++ it
  }

  def part1(n: Int): String = run().slice(n, n + 10).mkString

  def part2(seq: String) = {
    val s = seq.map(_.toInt - 48)
    run().sliding(seq.length).zipWithIndex.find(_._1 == s).head._2
  }

  println(part1(9), "5158916779")
//  println(part1(5), "0124515891")
//  println(part1(18), "9251071085")
//  println(part1(6), "9251071085")
 println(part1(input.toInt))
 println(part2("59414"), 2018)
 // println(part2("92510"), 18)
  println(part2(input))
  //println(run("37", 2018), "5941429882")

}
