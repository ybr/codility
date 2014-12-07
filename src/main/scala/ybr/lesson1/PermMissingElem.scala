package ybr

object PermMissingElem {
  def solution(A: Array[Int]): Int = {
    val sum = A.foldLeft(0)(_ + _)
    val max: Long = A.length + 1
    Math.abs(max * (max + 1) / 2 - sum).toInt
  }
}