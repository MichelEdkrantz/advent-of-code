package aoc2016

/**
  * Created by Michel Edkrantz on 2016-12-10.
  */
object Day7 extends App {
  val tlsTest = Seq(
      ("ioxxoj[asdfgh]zxcvbn[asdf]fdsa", true),
      ("abba[mnop]qrst", true),
      ("abcd[bddb]xyyx", false),
      ("aaaa[qwer]tyui", false),
      ("ioxxoj[asdfgh]zxcvbn", true)
  )

  val sslTest = Seq(
    ("aba[bab]xyz", true),
    ("xyx[xyx]xyx", false),
    ("aaa[kek]eke", true),
    ("zazbz[bzb]cdb", true)
  )

  val chars = Array('[', ']')
  def isReverseOk(s: String) = s == s.reverse && s(0) != s(1)
  def reversePositions(s: String) = s"${s(1)}${s(0)}${s(1)}"

  def letterGroups(window: Int, groups: Seq[String]) =
    groups.flatMap(_.sliding(window).map(_.mkString))

  def letterGroupOk(window: Int, groups: Seq[String]) =
    letterGroups(window, groups).exists(isReverseOk)

  def split(s: String) = {
    val split = s.split(chars).zipWithIndex.groupBy(_._2 % 2).mapValues(_.map(_._1))
    (split(0), split(1))
  }

  def supportsTLS(s: String) = {
    val (supernets, subnets) = split(s)
    !letterGroupOk(4, subnets) && letterGroupOk(4, supernets)
  }
  def supportsSSL(s: String) = {
    val (supernets, subnets) = split(s)
    val okSubnetsBabs = letterGroups(3, subnets).filter(isReverseOk).toSet
    letterGroups(3, supernets).filter(isReverseOk).map(reversePositions).exists(okSubnetsBabs.contains)
  }

  tlsTest.map(a => a._1 -> (supportsTLS(a._1) == a._2)).foreach(println)
  sslTest.map(a => a._1 -> (supportsSSL(a._1) == a._2)).foreach(println)
  val input = io.Source.fromFile("data/2016/day7.txt").getLines().toList.map(_.trim)
  println(input.count(supportsTLS))
  println(input.count(supportsSSL))

}
