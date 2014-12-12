package ybr.lesson3

object PassingCars {
  def solution(A: Array[Int]): Int = {
    val counts = A.foldLeft((0, 0)) { (prev, curr) =>
      val (zeros, ones) = prev
      if(curr == 0) (zeros + 1, ones) else (zeros, ones + 1)
    }

    val (zeros, ones) = counts
    val result = A.foldLeft((zeros, ones, 0)) { (prev, curr) =>
      val (zeros, ones, sum) = prev
      if(sum > 1000000000 || sum == -1)  (zeros, ones, -1)
      else if(curr == 0) (zeros - 1, ones, sum + ones)
      else (zeros, ones - 1, sum)
    }
    result._3
  }
}