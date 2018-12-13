package tools

object Tools {
  implicit class TupleAdd(t: (Int, Int)) {
    def +(p: (Int, Int)) = (p._1 + t._1, p._2 + t._2)
    def *(i: Int) = (t._1*i, t._2*i)
  }
}
