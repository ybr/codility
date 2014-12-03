package ybr

object PermCheck {
  def solution(A: Array[Int]): Int = {
    @annotation.tailrec
    def solutionRec(A: List[Int], sum: Int, once: Set[Int]): Int = {
      A match {
        case a::as =>
          if(once.contains(a)) 0
          else solutionRec(as, sum - a, once + a)
        case _ =>
          if(sum == 0) 1 else 0
      }
    }

    val sum = A.length * (A.length + 1) / 2d
    solutionRec(A.toList, sum.toInt, Set())
  }
}