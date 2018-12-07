package aoc2016


object ImplicitTuple {
  implicit class Tupple2Add[A : Numeric, B : Numeric](t: (A, B)) {
    import Numeric.Implicits._
    def + (p: (A, B)) = (p._1 + t._1, p._2 + t._2)
    def - (p: (A, B)) = (t._1 - p._1, t._2 - p._2)
  }
}
/**
  * Created by Michel Edkrantz on 2016-12-03.
  */
object Day2 extends App {
  import ImplicitTuple._

  type Position = (Int, Int)
  type Stepper = (Int, Char) => Int

  def stepOne(current: Int, direction: Char) = {
    def col(n: Int) = {
      val d = Math.floorMod(n, 3)
      if(d == 0) 3 else d
    }
    def row(n: Int) = Math.floorDiv(n + 2, 3)
    def minUp(n: Int) = col(n)
    def maxRight(n: Int) = row(n) * 3
    def minLeft(n: Int) = row(n) * 3 - 2
    def maxDown(n: Int) = col(n) + 6

    direction match {
      case 'U' => Math.max(current - 3, minUp(current))
      case 'R' => Math.min(current + 1, maxRight(current))
      case 'L' => Math.max(current - 1, minLeft(current))
      case 'D' => Math.min(current + 3, maxDown(current))
    }
  }

  class GridStepper(val grid: Array[Array[Int]]) {
    def getElem(pos: Position) = grid(pos._1)(pos._2)
    val index = (for {
      (arr, i) <- grid.zipWithIndex
      (el, j) <- arr.zipWithIndex
      if el != 0
    } yield {el -> (i,j)}).toMap

    def moveIfPossible(currentPos: Position, desired: Position): Position = {
      val p = try {
        if(getElem(desired) == 0) currentPos else desired
      } catch {
        case e: IndexOutOfBoundsException =>
          currentPos
      }
//      if(p != desired) {
//        println(s"Impossible to move to $desired")
//      }
      p
    }

    def step(current: Position, direction: Char): Position = direction match {
        case 'U' => moveIfPossible(current, current - (1,0))
        case 'R' => moveIfPossible(current, current + (0,1))
        case 'L' => moveIfPossible(current, current - (0,1))
        case 'D' => moveIfPossible(current, current + (1,0))
    }
  }

  def parseLine(line: String, seed: Int, stepper: Stepper): Int = {
      println(s"Got line $line with seed $seed")
      line.foldLeft(seed) {
        case (current, direction) =>
          //println(s"On current $current, getting instruction $direction")
          stepper(current, direction)
      }
  }

  def getCode(instructions: Iterable[String], seed: Int, stepper: Stepper): String = {
    instructions.scanLeft(seed) {
      case (seed, string) => parseLine(string, seed, stepper)
    }.drop(1).map(Integer.toHexString).mkString
  }

  val rawInstructions = if(false) {
    """
      |ULL
      |RRDDD
      |LURDL
      |UUUUD
    """.stripMargin
  } else {
    io.Source.fromFile("data/2016/day2.txt").mkString
  }

  val instructions = rawInstructions.split("\n").map(_.trim).filter(_.length > 0)

  val seed = 5
  //println(getCode(instructions, seed, stepOne))

  val grid = Array(
    Array(0,0,1,0,0),
    Array(0,2,3,4,0),
    Array(5,6,7,8,9),
    Array(0,10,11,12,0),
    Array(0,0,13,0,0)
  )
  val gridStepper = new GridStepper(grid)

  def parseLine2(line: String, seed: Int): Int = {
    //println(s"Got line $line with seed $seed")
    val pos = gridStepper.index(seed)
    val finalPos = line.foldLeft(pos) {
      case (current, direction) =>
        //println(s"On current $current=${gridStepper.getElem(current)}, getting instruction $direction")
        gridStepper.step(current, direction)
    }
    gridStepper.getElem(finalPos)
  }

  def getCode2(instructions: Iterable[String], seed: Int): String = {
    instructions.scanLeft(seed) {
      case (seed, string) =>
        val digit = parseLine2(string, seed)
        println(s"Got digit $digit")
        digit
    }.drop(1).map(Integer.toHexString).mkString
  }

  println(getCode2(instructions, seed))




}
