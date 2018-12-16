package aoc2018

import tools.Tools.Position
import tools.Tools.TupleMath

import scala.collection.mutable

object Game {
  def isAdjacent(p1: Position, p2: Position): Boolean = {
    val d = p1 - p2
    Math.abs(d._1) + Math.abs(d._2) == 1
  }
  val around = Seq((-1,0), (0,-1), (0, 1), (1,0)) // in read order
  def aroundPositions(p: Position) = around.map(_ + p)

}

class Game(initialState: Vector[Vector[Char]], elfPower: Int) {
  import Game._

  case class Creature(race: Char, var hp: Int = 200) {
    def isElf = race.equals('E')
  }

  val plainMap = initialState.map(_.map {
    case 'E' | 'G' => '.'
    case o => o
  })

  def isOpenGround(p: Position): Boolean = plainMap(p._1)(p._2) == '.'
  def inRangePositions(p: Position): Seq[Position] = aroundPositions(p).filterNot(isBlocked)

  def dijkstra1(source: Position): (Map[Position, Int], Map[Position, Position]) = {
    val active = mutable.Set(source)
    val res = mutable.Map(source -> 0)
    val pred = mutable.Map.empty[Position, Position]
    while (active.nonEmpty) {
      val node = active.minBy(res)
      active -= node
      val cost = res(node)
      val neighbors = aroundPositions(node).filterNot(isBlocked)
      for (n <- neighbors) {
        val cost1 = cost + 1
        if (cost1 < res.getOrElse(n, Int.MaxValue)) {
          active += n
          res += (n -> cost1)
          pred += (n -> node)
        }
      }
    }
    (res.toMap, pred.toMap)
  }

  val state = mutable.Map() ++ initialState.zipWithIndex.flatMap { case (line, i) =>
    line.zipWithIndex.flatMap {
      case (c, j) if c == 'E' | c == 'G' => Some((i, j) -> Creature(c))
      case _ => None
    }
  }
  def isBlocked(p: Position): Boolean = state.contains(p) || !isOpenGround(p)

  //both teams still alive
  def continue = state.groupBy(_._2.race).size > 1

  def shortestPaths(from: Position, to: Set[Position]) = {
    //expand using djikstra's algorithm
    val (_, pred) = dijkstra1(from)

    def shortestPath(end: Position): Seq[Position] = {
      var l = List(end)
      var prev = pred(end)
      while(prev != from) {
        l ::= prev
        prev = pred(prev)
      }
      l
    }
    //println(s"Got $visited")
    //println(s"Get shortest paths to positions: $to")
    to.toList.flatMap {
      case t if t == from => Some(List(from))
      case t if pred.contains(t) => Some(shortestPath(t))
      case _ => None
    }
  }

  def printMap(inRange: Set[Position] = Set.empty): Unit = {
    plainMap.zipWithIndex.foreach { case (row, i) =>
      val s = row.zipWithIndex.map { case (it, j) =>
        state.get((i,j)).map(_.race).getOrElse { if(inRange.contains((i,j))) "@" else it}
      }.mkString
      println(s)
    }
  }

  def attack(p: Position, enemy: Creature): Unit = {
    enemy.hp -= 3
    if(enemy.hp <= 0) {
     // println(s"Killed enemy $enemy at position $p")
      state.remove(p)
    } else {
     // println(s"Attacked enemy $enemy at position $p")
    }
  }

  def selectAdjacentEnemy(targets: Seq[(Position, Creature)]): Option[(Position, Creature)] = {
    targets.sortBy(t => (t._2.hp, t._1)).headOption
  }

  def getNextPos(p: Position, inRange: Set[Position]) = {
    if(inRange.isEmpty)
      None
    else {
      //if inrange in around positions, much easier...
      val a = aroundPositions(p).filterNot(isBlocked)
        .flatMap(ap => shortestPaths(ap, inRange).map(_.length).sorted.headOption.map(m => (m, ap)))
      a.sorted.headOption
    }
  }

  def run(): Unit = {
    printMap()
    var t = 0
    var unitsLeft = false
    while(continue) {
      t += 1
      println(s"\nAt t=$t")
      //calculate player order
      val order = state.iterator.toList.sortBy(_._1)

      val it = order.view.flatMap(o => state.get(o._1).filter(_ == o._2).map(o._1 -> _))

      //for each player if still alive
      it.foreach { case (pos, creature) =>
        //println(s"Eval $creature at $pos")
        //identify possible targets
        val targets = state.filter(_._2.race != creature.race)
        val adjacentEnemy = selectAdjacentEnemy(targets.filter(t => isAdjacent(t._1, pos)).toList)
        adjacentEnemy match {
          case Some((enemyPos, enemy)) => attack(enemyPos, enemy)
          case None if targets.isEmpty =>
              println("All enemies killed")
              unitsLeft = true
          case None =>
            //else find attackpositions to move to
            val inRange = targets.map(t => inRangePositions(t._1)).flatten.toSet
            //printMap(inRange)
            //println(s"Got inRange=$inRange")
            // calculate distance to all inRange positions (if reachable)
            getNextPos(pos, inRange).foreach { case (len, nextPos) =>
              //println(s"Move creature $creature from $pos to $nextPos, got l=$len")
              state(nextPos) = state.remove(pos).get
              if(len <= 2) {
                val adjacentEnemies = aroundPositions(nextPos)
                  .flatMap(p => state.get(p).filter(_.race !=creature.race).map(p -> _))
                selectAdjacentEnemy(adjacentEnemies).foreach {
                  //attack target in range
                  case (enemyPos, enemy) => attack(enemyPos, enemy)
                }
              }
            }
        }
      }
      printMap()
    }
    if(unitsLeft)
      t-=1

    val remainingHitPoints = state.map(_._2.hp).sum
    println(t, remainingHitPoints, t * remainingHitPoints)
  }

}

object Day15 extends App {

  val instructions = io.Source.fromFile("data/2018/day15.txt").getLines.map(_.toVector).toVector
  val g = new Game(instructions, elfPower = 3)
  g.run()
  //val sp = shortestPaths((3,2), Set((3,2)))
  //println(sp)
//  val inRange = Set((2,1))
 // val n = getNextPos2((3,1), inRange)
//  println(n)
}
