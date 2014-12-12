package ybr.lesson2

import scala.collection.immutable._

object MissingInteger {
  def solution(A: Array[Int]): Int = A.foldLeft(SortedSet((1 to (A.length + 1)).toSeq: _*))(_ - _).headOption.getOrElse(1)
}