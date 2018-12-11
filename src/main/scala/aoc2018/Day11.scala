package aoc2018

object Day11 extends App {
  val gridSize = 300
  val serial = 6392
  //val serial = 18

  def powerLevel(coord: (Int, Int), serial: Int) = {
    val rackId = coord._1 + 10
    val powerLevel = coord._2 * rackId.toLong
    val num = (powerLevel + serial) * rackId
    num / 100 % 10 - 5
  }

  def grid(serial: Int) = {
    (1 to gridSize).map ( y =>
      (1 to gridSize).map(x => powerLevel((x,y), serial))
    )
  }

  val g = grid(serial)

  def maxPower(windowSize: Int) = {
    val g1 = g.map(l => l.sliding(windowSize).map(_.sum).toVector)
    val g2 = g1.sliding(windowSize).map { lines =>
      lines.head.indices.map { i =>
        i -> lines.map(_.apply(i)).sum
      }
    }
    val max = g2.map(line => line.maxBy(_._2)).zipWithIndex.maxBy(_._1._2)
    val maxPower = max._1._2
    val coords = (max._1._1 +1, max._2 + 1)
    (maxPower, coords)
  }

  val samples = Seq(
    (3, 5, 8, 4),
    (122,79, 57, -5),
    (217,196, 39,  0),
    (101,153, 71,  4),
  )

  samples.foreach(input =>
    assert(powerLevel((input._1, input._2), input._3) == input._4)
  )

  val answer1 = maxPower(3)
  println(answer1)
  val answer2 =  (1 to gridSize).map(s => s -> maxPower(s)).maxBy(_._2._1)
  println(answer2)

}
