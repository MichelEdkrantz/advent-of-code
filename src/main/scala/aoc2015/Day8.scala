/**
  * Created by Michel Edkrantz on 2015-12-31.
  * How can we escape from hell?
  */
object Day8 extends App {

  val lines = scala.io.Source.fromFile("data/2015/day8.txt").getLines.toList

  val DOUBLE_BACKSLASH = """\\\\(.*)""".r //match \\ from santa
  val BACKSLASH = """\\(.*)""".r //match \ from santa
  val ESC_QUOTE = """\\"(.*)""".r // match \" from santa
  val QUOTE = "\"(.*)".r // match " in end/beginning
  val HEXA = """\\x\w{2}(.*)""".r
  val LETTERS = """(\w+)(.*)""".r

  def numChars(s: String): Int = s match {
    case "" => 0 //empty
    case LETTERS(letters, rest) => letters.length + numChars(rest)
    case HEXA(rest) => 1 + numChars(rest)
    case DOUBLE_BACKSLASH(rest) => 1 + numChars(rest)
    case ESC_QUOTE(rest) => 1 + numChars(rest)
    case QUOTE(rest) => numChars(rest)
    case default => throw new Exception("Unsupported: " + default)
  }

  def encode(s: String): String = {
    def enc(s:String): String = s match {
      case "" => "" //empty, the end
      case LETTERS(letters, rest) => letters + enc(rest)
      case BACKSLASH(rest) => "\\\\" + enc(rest)
      case QUOTE(rest) => "\\\"" + enc(rest)
      case default => throw new Exception("Unsupported: " + default)
    }
    "\"" + enc(s) + "\""
  }



  assert(numChars("") == 0)
  assert(numChars("abc") == 3)
  assert(numChars("\\x99") == 1)
  assert(numChars("abc\\x99") == 4)
  assert(numChars("\\\\") == 1) //single backslash, from santa \\
  assert(numChars("\\\\a") == 2) //single backslash, from santa \\
  assert(numChars("\\\"") == 1) //single qoute, from santa \"
  assert(numChars("\"a") == 1) //single qoute, from santa \"

  assert(encode("\"\"") == "\"\\\"\\\"\"")
  assert(encode("\"abc\"") == "\"\\\"abc\\\"\"")
  assert(encode("\"\\\"") == "\"\\\"\\\\\\\"\"") // "\" => "\"\\\""
  assert(encode("\"aaa\\\"aaa\"") == "\"\\\"aaa\\\\\\\"aaa\\\"\"")
  assert(encode("\"\\x27\"") == "\"\\\"\\\\x27\\\"\"")

  val nStringLiterals = lines.map(_.length).sum
  val nCharsInMem = lines.map(numChars).sum

  println(nStringLiterals)
  println(nCharsInMem)
  println(nStringLiterals - nCharsInMem)

  val nNewEncoding = lines.map(encode(_).length).sum

  print(nNewEncoding - nStringLiterals)

}
