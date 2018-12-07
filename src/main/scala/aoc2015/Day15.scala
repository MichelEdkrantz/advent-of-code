/**
  * Created by Michel Edkrantz on 2015-01-06.
  */

import scala.io.Source
import scala.util.Try

object Day15 extends App {

  val lines = Source.fromFile("data/2015/day15.txt").getLines().toSeq

  //Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
  val ingredientScores = lines map(line =>
    line.replace(",", "").split(" ").flatMap(a => Try(a.toInt).toOption)
  )


}

