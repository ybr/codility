package ybr

object FrogRiverOne {
  def solution(X: Int, A: Array[Int]): Int = {
    @annotation.tailrec
    def solutionRec(X: Int, A: List[Int], required: Set[Int], minute: Int): Int = {
      if(required.isEmpty) minute
      else A match {
        case a::as => solutionRec(X, as, required - a, minute + 1)
        case _ => -1
      }
    }

    solutionRec(X, A.toList, (1 to X).toSet, -1)
  }
}