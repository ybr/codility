package ybr.lesson2

import scala.collection.immutable._

object MaxCounters {
  def solution(N: Int, A: Array[Int]): Array[Int] = {
    // we must use a limit so we can increment one element after a max command has been run
    // without being forced to change all counters to the max
    val result = A.toList.foldLeft((Array.fill(N)(0), 0, 0)) { (prev, command) =>
      val (counters, max, limit) = prev
      val index = command - 1
      if(command >= N + 1) (counters, max, max)
      else {
        counters(index) = Math.max(counters(index), limit) + 1
        (counters, Math.max(counters(index), max), limit)
      }
    }
    result._1.map(Math.max(_, result._3))
  }
}