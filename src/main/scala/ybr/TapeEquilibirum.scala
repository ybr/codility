package ybr

import scala.collection.JavaConversions._

object TapeEquilibrium {
    def solution(A: Array[Int]): Int = {
        var lsum = A.head
        var rsum = A.tail.foldLeft(0)(_ + _)

        var diff = Math.abs(lsum - rsum)

        for(i <- 1 until A.length - 1) {
            lsum += A(i)
            rsum -= A(i)
            val newDiff = Math.abs(lsum - rsum)
            if(newDiff < diff) diff = newDiff
        }

        diff
    }
}