package aoc2018

import scala.collection.mutable

object Day13 extends App {
  val instructions = io.Source.fromFile("data/2018/day13.txt").getLines.map(_.toVector).toList

  import tools.Tools.TupleAdd

  case class DriverState(pos: (Int, Int), vel: (Int, Int), num: Int)
  case class CrashEvent(time: Int, driver: Int, pos: (Int, Int))

  val plainMap = instructions.map(_.map {
    case '^' | 'v' => '|'
    case '<' | '>' => '-'
    case l => l
  }).toVector

  val initialStates = instructions.zipWithIndex.flatMap { case (line, i) =>
    line.zipWithIndex.flatMap { case (c, j) =>
      (c match {
        case '^' => Some((-1, 0))
        case 'v' => Some((1, 0))
        case '>' => Some((0, 1))
        case '<' => Some((0, -1))
        case _ => None
      }) map (vel => DriverState((i, j), vel, 0))
    }
  }.zipWithIndex

  def turnLeft(vel: (Int, Int)) = (-vel._2, vel._1)
  def turnRight(vel: (Int, Int)) = (vel._2, -vel._1)

  def printState(states: List[DriverState]): Unit = {
    val driversByPosition = states.groupBy(_.pos)
    val charState = plainMap.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (it, x) =>
        driversByPosition.get((y, x)) match {
          case Some(s) if s.length > 1 => 'x'
          case Some(s :: Nil) => 'o' //no reverse mapping
          case None => it
        }
      }
    }
    charState.foreach(row => println(row.mkString))
  }

  def getNextDriverStateData(nextToken: Char,
                             vel: (Int, Int),
                             num: Int) = nextToken match {
    case '-' | '|' => (vel, num)
    case '/' if vel._1 == 0 => (turnLeft(vel), num)
    case '/' if vel._2 == 0 => (turnRight(vel), num)
    case '\\' if vel._1 == 0 => (turnRight(vel), num)
    case '\\' if vel._2 == 0 => (turnLeft(vel), num)
    case '+' => num match {
      case 0 => (turnLeft(vel), 1)
      case 1 => (vel, 2)
      case 2 => (turnRight(vel), 0)
    }
  }

  def runSimulation(cond: ((List[DriverState], List[CrashEvent])) => Boolean) = {
    val currentDrivers = mutable.Map[Int, DriverState]() ++ initialStates.map(_.swap)
    var crashEvents = List[CrashEvent]()
    Iterator.from(1).map { t =>
//      if(t % 100 == 0)
//        println(s"At step=$t")

      val sortedDriverIds = currentDrivers.toSeq.sortBy(_._2.pos).map(_._1)
        //filter out drivers that have been deleted already
        sortedDriverIds.view.filter(currentDrivers.contains).foreach { driver  =>
          val ds = currentDrivers.getOrElse(driver, null)
          val nextPosition = ds.pos + ds.vel

          val nextToken = plainMap(nextPosition._1)(nextPosition._2)

          // check if crash
          currentDrivers.find {
            case (d, p) => nextPosition == p.pos && d != driver
          } match {
            case Some((d, _)) =>
              //println(s"Got crash at XY=${nextPosition.swap}, removing drivers $d and $driver")
              crashEvents ::= CrashEvent(t, d, nextPosition)
              crashEvents ::= CrashEvent(t, driver, nextPosition)
              currentDrivers.remove(d)
              currentDrivers.remove(driver)
            case None =>
              val (nextVel, nextNum) = getNextDriverStateData(nextToken, ds.vel, ds.num)
              val d = DriverState(nextPosition, nextVel, nextNum)
              currentDrivers(driver) = d
          }
        }
        val driverState = currentDrivers.values.toList
        //printState(driverState)
      (driverState, crashEvents)
    }.find(cond).head
  }

  println("Answer 1")
  val answer1 = runSimulation(_._2.nonEmpty)._2.head.pos.swap
  println(answer1)

  println("Answer 2")
  val answer2 = runSimulation(_._1.size == 1)._1.head.pos.swap
  println(answer2)

}
