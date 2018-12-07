/**
  * Created by Michel Edkrantz on 2015-12-10.
  */
object Day2 extends App {
  val lines = scala.io.Source.fromFile("data/2015/day2.txt").getLines().toSeq
  val pattern = """(\d+)x(\d+)x(\d+)""".r
  val boxes = lines map {
    case pattern(h,l, w) => Seq(h,l,w).map(_.toInt).sorted
  }

  val wrappingPerBox = boxes map {
    case Seq(a, b, c) => 3*a*b + 2*b*c + 2*a*c
  }

  println(wrappingPerBox.sum)

  val ribbonPerBox = boxes map {
    case Seq(a,b,c) => a + a + b + b + a*b*c
  }

  println(ribbonPerBox.sum)
}
