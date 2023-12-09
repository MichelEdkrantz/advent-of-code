package aoc2023

object Day3 extends App {

  val instructions = scala.io.Source.fromFile("data/2023/day3.txt").getLines.toList
  val testInstructions = scala.io.Source.fromFile("data/2023/day3.test.txt").getLines.toList

  def isSymbol(sym: Char): Boolean = sym match {
    case '.' => false
    case n => !n.isDigit
  }
  assert(isSymbol('*'))
  assert(!isSymbol('.'))

  type BoolMat = Array[Array[Boolean]]

  def findLineDigitGroups(line: String): List[List[(Char, Int)]] = {
    val lineChars = line.toCharArray
    val ic = lineChars.zipWithIndex
    var groups: List[List[(Char, Int)]] = List()
    var currentGroup: List[(Char, Int)] = List()
    ic.foreach { case (c, i) =>
      if (c.isDigit) {
        currentGroup = currentGroup :+ (c, i)
      } else {
        if (currentGroup.nonEmpty) {
          groups :+= currentGroup
        }
        currentGroup = List()
      }
    }
    if (currentGroup.nonEmpty) {
      groups :+= currentGroup
    }
    groups
    //output groups of numbers with index
  }

  def groupHasSymbolAround(group: List[(Char, Int)], lineIndex: Int, boolMat: BoolMat): Boolean = {
    // this also checks chars in the group, but it doesn't matter
    val (first, last) = (group.head._2, group.last._2)
    val x = for {
      li <- lineIndex - 1 to lineIndex + 1
      ci <- first - 1 to last + 1
      if li >= 0 && li < boolMat.length && ci >= 0 && ci < boolMat(li).length
    } yield boolMat(li)(ci)
    x.contains(true)
  }

  val groups = findLineDigitGroups("467..114..")
  println(groups)

  def solve1(instructions: List[String]) = {
    val charMat = instructions.map(_.toCharArray).toArray
    val symbolMat = charMat.map(_.map(isSymbol))
    instructions.zipWithIndex.map { case (line, lineIndex) =>
      val groups = findLineDigitGroups(line)
      // walk around it and see if there is a symbol
      val b = groups.filter { groupHasSymbolAround(_, lineIndex, symbolMat) }
      b.map( gr => gr.map(_._1).mkString("").toInt).sum
    }.sum
  }

  val testAns = solve1(testInstructions)
  println(testAns)
  assert(testAns == 4361)
  //537732
  val realAns = solve1(instructions)
  println(realAns)

  def findLineGears(line: String): List[Int] = {
    line.toCharArray.zipWithIndex.filter(_._1 == '*').map(_._2).toList
  }

  // solve part 2
  // find all gear symbols.
  // then find all numbers around them
  def solve2(instructions: List[String]): Int = {
    val lineDigitGroups = instructions.map(findLineDigitGroups).toArray
    instructions.map(findLineGears).zipWithIndex.map { case (gears, lineIndex) =>
      // get a line with gear positions
      gears.map { gearIndex =>
       // println(s"gear at $lineIndex, $gearIndex")
        // check for any numbers around
        val digitGroupsAroundGear = (for {
          li <- lineIndex - 1 to lineIndex + 1
          if li >= 0 && li < instructions.length
          digitGroup <- lineDigitGroups(li)
          gi <- gearIndex - 1 to gearIndex + 1
          if gi >= 0 && gi < instructions(li).length
          if digitGroup.exists(_._2 == gi)
        } yield digitGroup).toSet
        if (digitGroupsAroundGear.size == 2) {
          val nums = digitGroupsAroundGear.map(_.map(_._1).mkString("").toInt)
          nums.head * nums.last
        } else 0
      }.sum
    }.sum

  }

  val testAns2 = solve2(testInstructions)
  println(testAns2)
  val ans2 = solve2(instructions)
  println(ans2)
  assert(ans2 == 84883664)
}
