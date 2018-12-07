/**
  * Created by Michel Edkrantz on 2015-01-06.
  */


object Day11 extends App {

  val myPuzzleInput = "cqjxjnds"

  val forbiddenLetterCheck = "[oil]".r
  val overlappingLetterCheck = "(.)(\\1)(.*)?(.)(\\4)(.*)".r

  def isValidPassword(password: String): Boolean = {
    //Passwords may not contain the letters i, o, or l,
    // as these letters can be mistaken for other characters and are therefore confusing.
    if(forbiddenLetterCheck.findFirstIn(password).isDefined) {
      return false
    }

    //Passwords must contain at least two different,
    // non-overlapping pairs of letters, like aa, bb, or zz.
    if (overlappingLetterCheck.findFirstIn(password).isEmpty) {
      return false
    }

    //Passwords must include one increasing straight of at least three letters,
    // like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
    var i = 0
    val stop = password.length - 2
    val intField = password.toCharArray.map(_.toInt)
    while(i < stop) {
      if(intField(i) == intField(i+1) - 1 && intField(i) == intField(i+2) - 2){
        return true
      }
      i += 1
    }
    false
  }

  def nextPassword(seed: String): String = {
    var current = incrementPassword(seed)
    while(!isValidPassword(current)){
      println(s"checking password: $current")
      current = incrementPassword(current)
    }
    current
  }

  def incrementPassword(password: String): String = {
    if(password.last != 'z'){
      password.substring(0, password.length-1) + (password.last+1).toChar
    } else {
      //last is z, we need to wrap around.
      //find the last non z, and increment it and make the rest of the z:s to a:s
      //flip the string to make greedy regex work
      val p = """(z+)([abcdefghijklmnopqrstuvwxy]*)(.*)""".r
      password.reverse match {
        case p(z, notZ, last) => {
          //println(notZ, z, last)
          var middle = ""
          if(notZ.length > 0) {
            middle = (notZ.charAt(0) + 1).toChar.toString
            if(notZ.length > 1)
              middle += notZ.substring(1, notZ.length)
          }
          ("a" * z.length + middle + last).reverse
        }
      }
    }
  }

  assert(incrementPassword("zaaa") == "zaab")
  assert(incrementPassword("aaaz") == "aaba")
  assert(incrementPassword("zzaz") == "zzba")
  assert(incrementPassword("zzzz") == "aaaa")

  assert(isValidPassword("hijklmmn") == false)
  assert(isValidPassword("abbceffg") == false)
  assert(isValidPassword("abbcegjk") == false)
  assert(isValidPassword("abcdffaa") == true)

  //println(nextPassword("abcdefgh"), "abcdffaa")
  //println(nextPassword("ghijklmn"), "ghjaabcc")
  //println(nextPassword(myPuzzleInput), "cqjxxyzz")
  println(nextPassword("cqjxxyzz"))

}
