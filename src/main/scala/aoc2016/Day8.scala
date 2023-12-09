package aoc2016

/**
  * Created by Michel Edkrantz on 2016-12-11.
  */
object Day8 extends App {
  type Screen = Array[Array[Boolean]]

  def printScreen(screen: Screen) = {
    screen.map(_.map(if(_) "#" else " ").mkString).foreach(println)
  }

  def rect(screen: Screen, a: Int, b: Int) = {
    for {
      x <- 0 until a
      y <- 0 until b
    } {
      screen(x)(y) = true
    }
  }

  def rotateRow(screen: Screen, row: Int, by: Int) = {
    screen(row) = (0 until screen.head.length).map { j =>
      screen(row)((j + screen.head.length - by) % screen.head.length)
    }.toArray
  }

  def rotateColumn(screen: Screen, col: Int, by: Int) = {
    val currentCol = screen.map(_(col))
    screen.zipWithIndex foreach { case (row,i) =>
      val idx = (i + screen.length - by) % screen.length
      row(col) = currentCol(idx)
    }
  }

  val rectPattern = "rect (\\d+)x(\\d+)".r
  val rotateRowPattern = "rotate row y=(\\d+) by (\\d+)".r
  val rotateColPattern = "rotate column x=(\\d+) by (\\d+)".r

  def applyInstructions(screen: Screen, instructions: Seq[String]) = {
    instructions foreach { inst =>
      println(s"\nApplying $inst")
      inst match {
        case rectPattern(width, height) => rect(screen, height.toInt, width.toInt)
        case rotateRowPattern(y, by) => rotateRow(screen, y.toInt, by.toInt)
        case rotateColPattern(x, by) => rotateColumn(screen, x.toInt, by.toInt)
      }
      printScreen(screen)
    }
  }

  def countLit(screen: Screen) = screen.map(_.count(identity)).sum

  val testInstructions = Seq(
    "rect 3x2",
    "rotate column x=1 by 1",
    "rotate row y=0 by 4",
    "rotate column x=1 by 1"
  )
  val testScreen = Array.fill[Boolean](3,7)(false)

  applyInstructions(testScreen, testInstructions)
  println(countLit(testScreen))

  val input = io.Source.fromFile("data/2016/day8.txt").getLines().toList
  val screen1 = Array.fill[Boolean](6,50)(false)
  applyInstructions(screen1, input)
  println(countLit(screen1))
}
