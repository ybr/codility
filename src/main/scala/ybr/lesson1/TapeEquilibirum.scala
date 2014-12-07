package ybr

import scala.collection.JavaConversions._

object TapeEquilibrium {
    def solution(A: Array[Int]): Int = {
        val lsum = A.head
        val rsum = A.tail.foldLeft(0)(_ + _)
        val diff = Math.abs(lsum - rsum)

        val response = A.tail.dropRight(1).foldLeft((lsum, rsum, diff)) { (prev, curr) =>
            val (prevLsum, prevRsum, prevDiff) = prev
            val lsum = prevLsum + curr
            val rsum = prevRsum - curr
            (lsum, rsum, Math.min(Math.abs(lsum - rsum), prevDiff))
        }

        response._3
    }
}