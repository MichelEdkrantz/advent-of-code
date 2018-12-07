import scala.collection.mutable

/**
  * Created by Michel Edkrantz on 2015-12-13.
  */
object Day6 extends App {
  val lines = scala.io.Source.fromFile("data/2015/day6.txt").getLines.toSeq

  val pattern = """^(.*) (\d+),(\d+) through (\d+),(\d+)$""".r

  def problem1() = {
    val matrix = new mutable.HashMap[(Int, Int), Boolean]()

    def toggle(x1: Int, y1: Int, x2: Int, y2: Int) = {
        for {
          x <- x1 to x2
          y <- y1 to y2
          key = (x,y)
        } {
          if(matrix.contains(key))
            matrix -= key
          else
            matrix += ((key, true))
        }
    }

    def turnOn(x1: Int, y1: Int, x2: Int, y2: Int) = {
      for {
        x <- x1 to x2
        y <- y1 to y2
        key = (x,y)
        if !matrix.contains(key)
      } matrix += ((key, true))
    }

    def turnOff(x1: Int, y1: Int, x2: Int, y2: Int) = {
      for {
        x <- x1 to x2
        y <- y1 to y2
        key = (x,y)
        if matrix.contains(key)
      } matrix -= key
    }

    def receiveCommand(s: String) = s match {
      case pattern(command, x1, y1, x2, y2) => command match {
        case("turn off") => turnOff(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        case("turn on") => turnOn(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        case("toggle") => toggle(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
      }
    }

    lines.foreach(receiveCommand)
    println(matrix.size) //377891
  }

  def problem2() = {
    val nRows, nCols = 1000
    val mat = Array.tabulate(nRows,nCols)( (x,y) => 0 )

    def command2(command: String, x1: Int, y1: Int, x2: Int, y2: Int) = {
      for {
        x <- x1 to x2
        y <- y1 to y2
      } {
        command match {
          case("turn off") =>
            mat(x)(y) = if(mat(x)(y) > 1) mat(x)(y) - 1 else 0
          case("turn on") =>
            mat(x)(y) += 1
          case("toggle") =>
            mat(x)(y) += 2
        }
      }
    }

    def receiveCommand2(s: String) = s match {
      case pattern(com, x1, y1, x2, y2) => command2(com, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }

    lines.foreach(receiveCommand2)

    print(mat.map(_.sum).sum)
  }

  problem1()
  problem2()


}