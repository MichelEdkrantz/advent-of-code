import scala.collection.mutable

/**
  * Created by Michel Edkrantz on 2015-12-13.
  */
object Day7 extends App {

  val THREE_COMMAND = """(.+) (RSHIFT|LSHIFT|AND|OR) (.+)""".r
  val NOT = """NOT (.+)""".r
  val ASSIGN = """(.+)""".r

  val REGISTER = """(\w+)""".r
  val DIGIT = """(\d+)""".r

  val lines = scala.io.Source.fromFile("data/2015/day7.txt").getLines.map(_.trim).toList

  val registers = mutable.Map[String, Char]()

  val instructions = mutable.Map() ++ (lines.map { line => {
    line.split(" -> ").toList match {
      case List(a, b) => b -> a
      case _ => throw new Exception("Could not split")
    }
  }}.toMap)

  def resolve(op: String): Option[Char] = op match {
    //if parsing digit, we are done
    case DIGIT(d) => Some(d.toInt.toChar)
    //else, need to find value recursively
    case REGISTER(reg) => resolveFromRegister(reg)
    case _ => throw new Exception("could not match " + op)
  }

  def resolveFromRegister(reg: String): Option[Char] = {
    registers.get(reg) match {
      case Some(a) => Some(a) //we already have a memoized value
      case None => {
        //memoized evalution
        instructions.get(reg) match {
          case Some(instruction) =>
            registers(reg) = evalInstruction(instruction)
            registers.get(reg)
        }
      }
    }
  }

  def evalInstruction(instruction: String): Char = instruction match {
    case THREE_COMMAND(op1, command, op2) => {
      val a = resolve(op1).get
      val b = resolve(op2).get
      command match {
        case "RSHIFT" => (a >> b).toChar
        case "LSHIFT" => (a << b).toChar
        case "AND" => (a & b).toChar
        case "OR" => (a | b).toChar
      }
    }
    case NOT(from) => resolve(from) match {
      case Some(fr: Char) => (~fr).toChar
    }
    case ASSIGN(from) => resolve(from) match {
      case Some(fr: Char) => fr
    }
    case _ => throw new Exception("Unsupported case " + instruction)
  }

  registers.toSeq.sorted.foreach(a => println(a._1 + ": " + a._2.toInt))

  val aSignal = resolve("a").get.toInt
  println(aSignal) //46065

  //reset
  registers.clear()
  instructions("b") = aSignal.toString
  val aSignal2 = resolve("a").get.toInt
  println(aSignal2)
}
