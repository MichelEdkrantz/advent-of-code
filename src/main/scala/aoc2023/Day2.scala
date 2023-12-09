package aoc2023

object Day2 extends App {

  val LineParser = "Game (\\d+): (.*)".r
  val Red = "(\\d+) red".r
  val Blue = "(\\d+) blue".r
  val Green = "(\\d+) green".r

  case class DiceThrow(red: Int, green: Int, blue: Int) {
    def +(other: DiceThrow): DiceThrow = DiceThrow(red + other.red, green + other.green, blue + other.blue)
    override def toString: String = "DiceThrow(red=%d, green=%d, blue=%d)".format(red, green, blue)
  }

  case class GameLine(gameId: Int, diceThrows: Vector[DiceThrow])

  def parseLine(line: String): GameLine = line match {
    case LineParser(game, scores) =>
      val diceThrows = scores.split(";")
      val dts = diceThrows.map { dt =>
        dt.split(",").map(_.trim).foldLeft(DiceThrow(0, 0, 0)) {
          case (acc, d) => d match {
            case Red(n) => acc + DiceThrow(n.toInt, 0, 0)
            case Green(n) => acc + DiceThrow(0, n.toInt, 0)
            case Blue(n) => acc + DiceThrow(0, 0, n.toInt)
          }
        }
      }
      GameLine(game.toInt, dts.toVector)
  }

  val max = DiceThrow(12, 13, 14)

  val instructions = scala.io.Source.fromFile("data/2023/day2.txt").getLines.toList.map(parseLine)
  val testInstructions = scala.io.Source.fromFile("data/2023/day2.test.txt").getLines.toList.map(parseLine)

  def solve1(gameLines: List[GameLine]) = {
    def gameOk(gameLine: GameLine): Boolean = {
      gameLine.diceThrows.forall { dt =>
        dt.red <= max.red && dt.green <= max.green && dt.blue <= max.blue
      }
    }
    val okGames = gameLines.filter(gameOk)
    okGames.map(_.gameId).sum
  }

  val testAns1 = solve1(testInstructions)
  println(testAns1)

  val ans1 = solve1(instructions)
  println(ans1)

  def getGamePower(gameLine: GameLine): Int = {
    val m = gameLine.diceThrows.foldLeft(DiceThrow(0,0,0)) { case (a, b) =>
      DiceThrow(Math.max(a.red, b.red), Math.max(a.green, b.green), Math.max(a.blue, b.blue))
    }
    m.red * m.green * m.blue
  }

  def solve2(gameLines: List[GameLine]) = {
    gameLines.map(getGamePower).sum
  }

  val testAns2 = solve2(testInstructions)
  println(testAns2)

  val ans2 = solve2(instructions)
  println(ans2)



}
