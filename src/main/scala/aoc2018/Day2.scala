package aoc2018

object Day2 extends App {
  val instructions = io.Source.fromFile("data/2018/day2.txt").getLines().toList

  def countsPerLetter(str: String): Map[Char,Int] = str.groupBy(c => c).mapValues(_.size)

  //create inverted with letters for different counts
  def countsMap(str: String) = countsPerLetter(str).groupBy(_._2).mapValues(_.keys.toSet)

  def partA(): Unit = {

    val counts = instructions.map(countsMap)

    val two = counts.count(_.contains(2))
    val three = counts.count(_.contains(3))
    val answer1 = two * three
    print(answer1)
  }

  def almostEqual(str1: Array[Char], str2: Array[Char]) = {
    str1.zip(str2).count(s => s._1 != s._2) == 1
  }

  def partB(): Unit = {
    instructions.map(_.toCharArray).combinations(2).filter {
      a => almostEqual(a(0), a(1))
    }.foreach { a =>
      println(s"Got boxes ${a(0).mkString} and ${a(1).mkString}")
      //find the diff between two strings now
      val commonLetters = a(0).zip(a(1)).filter(s => s._1 == s._2).map(_._1).mkString
      println(commonLetters)
    }
  }

  partA()
  partB()


}
