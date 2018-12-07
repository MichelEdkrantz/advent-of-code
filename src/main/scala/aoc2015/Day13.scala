/**
  * Created by Michel Edkrantz on 2015-01-06.
  */

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {

  type AdjMatrix = Array[Array[Int]]

  val lines = Source.fromFile("data/2015/day13.txt").getLines().toSeq

  def buildHappinessMatrix(lines: Seq[String]): (Vector[String], AdjMatrix) = {
    val guestPattern = """(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).""".r

    val mappings = lines.map({
      case guestPattern(personA, verb, happiness, personB) =>
        val hap = if(verb == "gain") happiness.toInt else -happiness.toInt
        (personA, personB, hap)
    })

    val persons = mappings.flatMap(d => List(d._1, d._2)).toSet.toVector

    val adjMatrix = Array.ofDim[Int](persons.length, persons.length)
    mappings.foreach {
      case(personA, personB, happiness) =>
        val indA = persons.indexOf(personA)
        val indB = persons.indexOf(personB)
        adjMatrix(indA)(indB) = happiness
    }

    (persons, adjMatrix)
  }

  //this is essentially the same as Day9 TSP, but we seek a connected path
  def optimalHappiness(matrix: AdjMatrix): (Seq[Int], Int) = {

    def combinedHappiness(a: Int, b: Int) = matrix(a)(b) + matrix(b)(a)

    //solve by recursion
    def happiness(seq: Seq[Int], totalHappiness: Int): (Seq[Int], Int) = {
      if (seq.length == matrix.length) {
        (seq, totalHappiness + combinedHappiness(seq.head, seq.last)) //we are done
      } else {
        //visit all branches not yet visited
        val unvisited = (0 until matrix.length).filterNot(seq.contains(_))
        val seqs = unvisited map { i =>
          happiness(seq :+ i, totalHappiness + combinedHappiness(i, seq.last))
        }
        seqs.zipWithIndex.maxBy(_._1._2)._1
      }
    }
    happiness(Seq(0),0)
  }

  val (persons, matrix) = buildHappinessMatrix(lines)
  println(optimalHappiness(matrix))
  val (maxPath, max) = optimalHappiness(matrix)
  println("Max", maxPath, max)
  println(maxPath.map(persons(_)))

  //part2
  val personsWithMe = persons :+ "Me"
  val matrix2 = Array.ofDim[Int](personsWithMe.length, personsWithMe.length)

  for {
    i <- 0 until matrix.length
    j <- 0 until matrix.length

  } matrix2(i)(j) = matrix(i)(j)

  val (maxPath2, max2) = optimalHappiness(matrix2)
  println("Max", maxPath2, max2)
  println(maxPath2.map(personsWithMe(_)))



}

