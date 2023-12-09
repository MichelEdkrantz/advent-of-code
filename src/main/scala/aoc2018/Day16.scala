package aoc2018

import scala.annotation.tailrec

object Day16 extends App {

  val instructionsA = io.Source.fromFile("data/2018/day16a.txt").getLines().map {
    line => line.split(" ").map(_.toInt).toVector.grouped(4).toVector
  }.toList

  val instructionsB = io.Source.fromFile("data/2018/day16b.txt").getLines().map {
    line => line.split(" ").map(_.toInt).toVector
  }.toList

  val opNames = "addr,addi,mulr,muli,banr,bani,borr,bori,setr,seti,gtir,gtri,gtrr,eqir,eqri,eqrr".split(",").toVector

  def applyInstruction(op: String, value: Vector[Int], reg: Vector[Int]): Vector[Int] = {
    val (a, b, c) = (value(1), value(2), value(3))
    val out = reg.toBuffer
    out(c) = op match {
      //addr (add register) stores into register C the result of adding register A and register B.
      case "addr" => reg(a) + reg(b)
      //addi (add immediate) stores into register C the result of adding register A and value B.
      case "addi" => reg(a) + b
      //mulr (multiply register) stores into register C the result of multiplying register A and register B.
      case "mulr" => reg(a) * reg(b)
      //muli (multiply immediate) stores into register C the result of multiplying register A and value B.
      case "muli" => reg(a) * b
      //banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
      case "banr" => reg(a) & reg(b)
      //bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
      case "bani" => reg(a) & b
      //borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
      case "borr" => reg(a) | reg(b)
      //bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
      case "bori" => reg(a) | b
      //setr (set register) copies the contents of register A into register C. (Input B is ignored.)
      case "setr" => reg(a)
      //seti (set immediate) stores value A into register C. (Input B is ignored.)
      case "seti" => a
      //gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
      case "gtir" => if(a > reg(b)) 1 else 0
      //gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
      case "gtri" => if(reg(a) > b) 1 else 0
      //gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
      case "gtrr" => if(reg(a) > reg(b)) 1 else 0
      //eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
      case "eqir" => if(a == reg(b)) 1 else 0
      //eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
      case "eqri" => if(reg(a) == b) 1 else 0
      //eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
      case "eqrr" => if(reg(a) == reg(b)) 1 else 0
    }
    out.toVector
  }

  def test(): Unit = {
    val test = (Vector(3, 2, 1, 1), Vector(9, 2, 1, 2), Vector(3, 2, 2, 1))
    val resp = opNames.map(o => o -> applyInstruction(o, test._2, test._1))
    println(resp.toMap)
    val matches = resp.filter(_._2 == test._3)
    println(s"Matches $matches")
    val nMatches = resp.count(_._2 == test._3)
    println(s"Test nMatches=$nMatches")
  }

  def part1(): Unit = {
    val matchesPerInstruction = instructionsA.map { i =>
      opNames.map(o => applyInstruction(o, i(1), i(0))).count(_ == i(2))
    }
    println(matchesPerInstruction.count(_ >= 3))
  }

  //recursive is beautiful
  @tailrec
  def resolve(known: Map[String, Int], rest: Map[String, Set[Int]]): Map[String, Int] = {
    if(rest.isEmpty) known
    else {
      val (next, codes)  = rest.find(_._2.size == 1).head
      resolve(
        known ++ Map(next -> codes.head),
        rest.filter(_._1 != next).view.mapValues(_ - codes.head).toMap
      )
    }
  }

  def part2(): Unit = {
    val candidateMappings = instructionsA.flatMap { i =>
      opNames.flatMap { op =>
        val out = applyInstruction(op, i(1), i(0))
        if(out == i(2)) Some(op, i(1)(0)) else None
      }
    }
    val stringToInts = candidateMappings.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap
    val intToName = resolve(Map.empty, stringToInts).map(_.swap)

    val finalRegistry = instructionsB.foldLeft(Vector(0,0,0,0)) { case (reg, input) =>
      applyInstruction(intToName(input(0)), input, reg)
    }
    println(finalRegistry)
  }

  part1()
  part2()
}
