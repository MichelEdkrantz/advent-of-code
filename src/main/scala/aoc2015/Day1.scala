/**
  * Created by Michel Edkrantz on 2015-12-10.
  */
object Day1 extends App {

  val steps = scala.io.Source.fromFile("data/2015/day1.txt").mkString.map {
      case '(' => 1
      case ')' => -1
      case _ => 0
  }

  //To what floor do the instructions take Santa?
  println(steps.sum)

  //What is the position of the character that causes Santa to first enter the basement?
  val levels = steps.scanLeft(0) { (total, n) => total+n }
  val index = levels.indexOf(-1, 1)
  println(index)

}
