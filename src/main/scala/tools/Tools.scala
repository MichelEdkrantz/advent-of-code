package tools

object Tools {
  type Position = (Int, Int)
  implicit class TupleMath(t: Position) {
    def +(p: Position) = (p._1 + t._1, p._2 + t._2)
    def -(p: Position) = (t._1 - p._1, t._2 - p._2)
    def *(i: Int) = (t._1*i, t._2*i)
  }
}
