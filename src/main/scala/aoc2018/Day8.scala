package aoc2018

object Day8 extends App {
  val instructions = io.Source.fromFile("data/2018/day8.txt").mkString.split(" ").map(_.toInt)
  case class Node(children: Seq[Node], metadata: Seq[Int])

  def parse(seq: Seq[Int]): (Node, Seq[Int]) = {
    val (nChildren, nMetadata, rest) = (seq.head, seq(1), seq.drop(2))

    //parse children recursively and pass the unknown tail
    val (children, right) = (1 to nChildren).foldLeft((Seq[Node](), rest)) { case ((kids, left), _) =>
      val (n, rest) = parse(left)
      (kids :+ n, rest)
    }

    val (metadata, next) = right.splitAt(nMetadata)
    (Node(children, metadata), next)
  }

  def sum1(n: Node): Int = {
    n.metadata.sum + n.children.map(sum1).sum
  }

  def sum2(n: Node): Int = {
    if(n.children.isEmpty) n.metadata.sum
    else n.metadata.filter(_ <= n.children.size).map(_ - 1).map(n.children.apply).map(sum2).sum
  }

  val baseNode = parse(instructions)._1

  val answer1 = sum1(baseNode)
  val answer2 = sum2(baseNode)
  println(answer1)
  println(answer2)
}
