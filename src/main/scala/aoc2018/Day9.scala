package aoc2018

object Day9 extends App {

  //using linked list operations here is crucial for performance
  def play(nPlayers: Int, nMarbles: Int) = {
    val res = (2 to nMarbles).foldLeft((List[Int](1,0), List[Int](), 1, List.empty[(Int, Int)])) {
      case ((head, tail, player, scores), marbleNumber) =>

//        if(marbleNumber < 100) {
//          val correctOrder = head.reverse ::: tail
//          println(s"[${marbleNumber - 1}] ${correctOrder.mkString(" ")}")
//        }

        val nextPlayer = (player + 1) % nPlayers

        if (marbleNumber % 23 == 0) {
          //if more than 7 elements in head
          val (h1, h2) = head.splitAt(7)
          val (newHead, newTail, removed) = if(h1.length == 7) {
            val h1r = h1.reverse
            (h1r.head :: h2.tail, h1r.tail ::: tail, h2.head)
          } else {
            //h2 is empty
            //needs expensive rebalance, will get to the end of the tail
            val revTail = tail.reverse
            //take the last few items from the tail
            val (t1, t2) = revTail.splitAt(7-h1.length)
            val t1r = t1.reverse
            (t2 ::: h1, t1r.tail, t1r.head)
          }

          val newScores = (player, marbleNumber + removed) :: scores
          (newHead, newTail, nextPlayer, newScores)
        } else {
          val (newHead, newTail) = tail.headOption match {
            case Some(tailHead) =>
              //there are still more elements
              (marbleNumber :: tailHead :: head, tail.tail)
            case None =>
              //we are at the end of the list
              val rev = head.reverse
              (marbleNumber :: rev.head :: Nil, rev.tail)

          }
          (newHead, newTail, nextPlayer, scores)
        }
    }
    val scores = res._4.groupBy(_._1).mapValues(_.map(_._2.toLong).sum)
    val winner = scores.maxBy(_._2)
    winner._2
  }

  val testInput = Array(
    (9, 25, 32),
    (10, 1618, 8317),
    (13, 7999, 146373),
    (17, 1104, 2764),
    (21, 6111, 54718),
    (30, 5807, 37305)
  )

  def checkPlay(input: (Int, Int, Int)) = {
    val (p, m, ms) = input
    val score = play(p, m)
    println(s"Got input p=${p}, m=$m, ms=$ms = $score. ${if(ms == score) "OK" else "FAIL"}")
  }

  checkPlay(testInput(0))

  testInput.foreach(checkPlay)
  val answer1 = play(425, 70848)
  println(answer1)
  val answer2 = play(425, 70848*100)
  println(answer2)
}