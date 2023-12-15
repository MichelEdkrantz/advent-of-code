package aoc2023

import tools.AocDay

object Day7 extends App with AocDay {
  val (year, day) = (2023, 7)

  def parseLine(str: String) = str match {
    case s"$hand $bid" => Hand(hand, bid.toInt)
  }

  case class Hand(str: String, bid: Int) {
    val countByKey = cards.groupBy(identity).view.mapValues(_.size)
    lazy val cards: Vector[Int] = cardsFromStr(str)
    lazy val strength = getStrength(this)
  }

  def cardsFromStr(str: String) = str.toCharArray.map {
    case 'A' => 14
    case 'K' => 13
    case 'Q' => 12
    case 'J' => 11
    case 'T' => 10
    case d => d.asDigit
  }.toVector

  def getStrength(h: Hand): Int = h.countByKey.size match {
    // five of a kind
    case 1 => 10
    // four of a kind
    case 2 if h.countByKey.values.exists(_ == 4) => 9
    // full house
    case 2 => 8
    // three of a kind
    case 3 if h.countByKey.values.exists(_ == 3) => 7
    // two pairs
    case 3 if h.countByKey.values.count(_ == 2) == 2 => 6
    // pair
    case 4 => 5
    // all different
    case _ => 1
  }

  def handSorter(left: Hand, right: Hand): Boolean = {
    if (left.strength == right.strength) {
      val firstDiff = left.cards.zip(right.cards).find(x => x._1 != x._2)
      firstDiff.exists(x => x._1 > x._2)
    } else {
      left.strength > right.strength
    }
  }

  def solveProblem1(hands: Seq[Hand]) = {
    val sorted = hands.sortWith(handSorter)
    sorted foreach { h =>
      println(s"Hand ${h.str} has strength=${h.strength}")
    }
    sorted.reverse.zipWithIndex.foldLeft(0) { case (acc, (hand, i)) =>
      acc + (i + 1) * hand.bid
    }
  }

  val testInput = testInstructions.map(parseLine)
  val input = instructions.map(parseLine)
  println(solveProblem1(testInput))
  println(solveProblem1(input))


}
