package aoc2023

import scala.collection.mutable

object Day4 extends App {

  case class Card(id: Int, winning: Vector[Int], played: Vector[Int]) {
    val nMatches = played.count(winning.contains)
    val score = if (nMatches == 0) 0 else 1 << (nMatches - 1)
  }
  val LineParser = "Card\\s*(\\d+):([\\s\\d]+) \\| ([\\s\\d]+)".r
  def parseCard(line: String): Card = line match {
    case LineParser(card, winning, second) =>
      Card(card.toInt,
        winning.trim.split("\\s+").map(_.toInt).toVector,
        second.trim.split("\\s+").map(_.toInt).toVector)
  }

  val instructions = scala.io.Source.fromFile("data/2023/day4.txt").getLines.toList.map(parseCard)
  val testInstructions = scala.io.Source.fromFile("data/2023/day4.test.txt").getLines.toList.map(parseCard)

  println(testInstructions)

  val testAns1 = testInstructions.map(_.score).sum
  println(testAns1)

  val ans1 = instructions.map(_.score).sum
  println(ans1)

  def solve2(cards: Array[Card]): Int = {
    val numCards: mutable.Map[Int, Int] = mutable.Map() ++ cards.map(c => c.id -> 1)
    for {
      card <- cards
      ci = card.id
      if card.nMatches > 0
      i <- ci +1 to ci + card.nMatches
      if i <= cards.length
    } {
      numCards(i) = numCards(i) + numCards(ci)
    }
    numCards.values.sum
  }

  val testAns2 = solve2(testInstructions.toArray)
  println(testAns2)
  val ans2 = solve2(instructions.toArray)
  println(ans2)



}
