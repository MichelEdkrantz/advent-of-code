package aoc2016

import java.security.MessageDigest

object Day5 extends App {
  val token = "wtnhxymk"
  val test_token = "abc"
  val hasher = MessageDigest.getInstance("MD5")
  def toHex(b: Byte) = "%02x".format(b)
  def md5(text: String) = hasher.digest(text.getBytes)
  def hexstring(bytes: Array[Byte]) = bytes.map(toHex).mkString
  def has5zeroes(a: Array[Byte]) = a(0) == 0 && a(1) == 0 && a(2) > -1 && a(2) < 16

  val has5zerosStream = Stream.from(0).map(i => md5(s"$token$i")).filter(has5zeroes)

  val password1 = has5zerosStream.map(arr => toHex(arr(2))(1)).take(8).mkString
  println(password1)

  var pwd: Array[Option[Char]] = Array.fill(8)(None)
  val p = has5zerosStream.filter { arr =>
    toHex(arr(2))(1) match {
      case x if x >= '0' && x <= '7' => {
        val token = toHex(arr(3))(0)
        println(hexstring(arr))
        println(s"Got token=$token at position $x")
        val pos = s"$x".toInt
        pwd(pos) match {
          case Some(_) => false
          case None =>
            pwd(pos) = Some(token)
            true
        }
      }
      case _ => false
    }
  }.take(8).mkString
  print(pwd.flatten.mkString)



}
