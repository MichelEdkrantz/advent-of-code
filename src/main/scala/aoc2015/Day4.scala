import java.security.MessageDigest

import scala.util.control.Breaks._
/**
  * Created by Michel Edkrantz on 2015-12-11.
  */
object Day4 extends App {

  val seed = "iwrupvqb"

  val messageDigest = MessageDigest.getInstance("MD5")

  def md5(s: String) = messageDigest.digest(s.getBytes).map("%02x".format(_)).mkString

  def check(s: String, zeros: Int): Boolean = md5(s).substring(0,zeros) == "0"*6

  //this is painfully slow
  def find(seed: String, zeros: Int): Unit = {
    breakable { for (i <- 1 to 10000000) {

      if(i % 1000 == 0)
        println(seed + i)

      val b = check(seed + i, zeros)
      if(b) {
        println("Match for " + seed + i)
        break
      }
    } }
  }

  find(seed, 6)
}
