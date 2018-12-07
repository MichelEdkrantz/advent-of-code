/**
  * Created by Michel Edkrantz on 2015-01-06.
  */

import scala.io.Source

object Day14 extends App {

  val lines = Source.fromFile("data/2015/day14.txt").getLines().toSeq

  //Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
  val pattern = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  case class Raindeer(name: String, speed: Int, dexterity: Int, mustRest: Int) {
    private var canFlyIn = 0
    private var hasFlownIn = 0

    var distance = 0

    def raceOneStep(): Unit ={
      if(canFlyIn == 0){
        distance += speed
        hasFlownIn += 1
      } else {
        canFlyIn -= 1
      }
      if(hasFlownIn == dexterity){
        canFlyIn = mustRest
        hasFlownIn = 0
      }
    }
  }

  val raindeers = lines map {
    case pattern(name, speed, time, rest) => Raindeer(name, speed.toInt, time.toInt, rest.toInt)
  }

  val raceTime = 2503
  val leader = Array.ofDim[Raindeer](raceTime)
  for {
    i <- 0 until raceTime
  } {
    // println(s"${r.name} now at position ${r.distance}")
    raindeers.foreach(_.raceOneStep())
    leader(i) = raindeers.maxBy(_.distance)
  }
  val scoreBoard = leader.groupBy(identity).mapValues(_.size)

  val best = raindeers.maxBy(_.distance)
  println(best, best.distance)

  print(scoreBoard)


}

