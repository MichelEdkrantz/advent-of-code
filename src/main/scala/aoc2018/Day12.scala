package aoc2018

object Day12 extends App {
  val instructions = io.Source.fromFile("data/2018/day12.txt").getLines

  val initialState = instructions.next().map(_ == '#').zipWithIndex.map(s => s._2.toLong -> s._1)
  val regex = "(.*) => (.*)".r
  val patterns = instructions.map {
    case regex(a, b) => (a.map(_ == '#'), b == "#")
  }.toList
  val positivePatterns = patterns.filter(_._2).map(_._1)

  def toString(state: Iterable[Boolean]) = state.map {
    case true => '#'
    case false => '.'
  }.mkString

  def nextState(window: Iterable[Boolean]) = positivePatterns.exists(_ == window)
  def stateSum(st: Iterable[(Long, Boolean)]) = st.filter(_._2).map(_._1).sum

  def grow(nGens: Long): Long = {
    val st = (1L to nGens).foldLeft(initialState) { case (state, gen) =>

      //this is a little verbose and ugly
      val i = state.head._1
      val j = state.last._1
      val head = ((i-5) until i).map(i => (i, false))
      val tail = ((j+1) to (j+4)).map(i => (i, false))
      val ext = head ++ state ++ tail
      val tmp = ext.sliding(5).map { s =>
        s(2)._1 -> nextState(s.map(_._2))
      }.toVector
      val st = tmp.dropWhile(it => !it._2).reverse.dropWhile(it => !it._2).reverse
      if(gen % 100 == 0) {
        println(s"At gen=$gen, i=$i j=$j, stateSum=${stateSum(st)}")
      }
      st
    }
    //st.map(s => toString(s.map(_._2))).foreach(println)
    stateSum(st)
  }

  val answer1 = grow(20)
  println(answer1)

  //after a few iterations, we reach a steady state of plants that move right, let's extrapolate the sum!
  // (like in conveys game of life)
  val nGens2 = 50000000000L
  def extrapolate(base: Long, inc: Long, n: Long): Long = {
    base + inc*n
  }

  val (y1, y2) = (200, 300)
  val (g1, g2) = (grow(y1), grow(y2))
  val diff = g2 - g1
  val scale = y2-y1
  val answer2 = extrapolate(g1, diff, (nGens2-y1)/scale)
  println(answer2)



}
