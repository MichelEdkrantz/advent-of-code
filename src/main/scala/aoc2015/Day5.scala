/**
  * Created by Michel Edkrantz on 2015-12-13.
  */
object Day5 extends App {

  var VOWELS = "aeiou"
  val FORBIDDEN_PATTERN = "(ab|cd|pq|xy)".r
  def isVowel(c: Char) = VOWELS.indexOf(c) > -1

  def isNice(s: String): Boolean = {

    //It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
    if(s.count(isVowel) < 3) return false

    //It contains at least one letter that appears twice in a row
    if(s.sliding(2).count(s => s(0) == s(1)) < 1) return false

    //It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
    FORBIDDEN_PATTERN.findFirstIn(s) match {
      case Some(x) => return false
      case None => return true
    }
  }

  def isNice2(s: String): Boolean = {
    // It contains a pair of any two letters that appears at least twice
    // in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa),
    // but not like aaa (aa, but it overlaps).
    val ok = s.sliding(3,2).flatMap { _.sliding(2,1).toSeq.distinct }.toList.groupBy(identity).filter(_._2.size > 1).size > 0

    if(!ok) return false

    // It contains at least one letter which repeats with exactly one letter between them,
    // like xyx, abcdefeghi (efe), or even aaa.
    return s.sliding(3,1).filter(a => a(0) == a(2)).size > 0
  }

  val doublePairLetters =  """(..).*\1""".r
  val repeat =  """(.).\1""".r

  def isNice3(s: String) = doublePairLetters.findFirstIn(s).nonEmpty && repeat.findFirstIn(s).nonEmpty

  def test(): Unit = {
    //tests
    assert(isNice("ugknbfddgicrmopn") == true) //nice
    assert(isNice("aaa") == true) //nice
    assert(isNice("jchzalrnumimnmhp") == false) //naughty
    assert(isNice("haegwjzuvuyypxyu") == false) //naughty
    assert(isNice("dvszwmarrgswjxmb") == false) //naughty

    assert(isNice3("xxyxx") == true) //nice

    assert(isNice3("xxxxx") == true) //nice

    //is nice because it has a pair that appears twice and a letter that repeats with one between,
    // even though the letters used by each rule overlap.
    assert(isNice3("qjhvhtzxzqqjkmpb") == true) //nice

    assert(isNice3("askbbywyyykerghp") == false) //nice
    //uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
    assert(isNice3("uurcxstgmygtbstg") == false) //naughty
    //ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.
    assert(isNice3("ieodomkazucvgmuy") == false) //naughty
  }


  val lines = scala.io.Source.fromFile("data/2015/day5.txt").getLines.map(_.trim).toSeq
  println(lines.count(isNice))
  println(lines.count(isNice3))

  lines.filter(a => isNice2(a) != isNice3(a)).foreach(println)

}
