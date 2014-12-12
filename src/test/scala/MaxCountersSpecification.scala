package ybr.lesson2

import scala.util.Random

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object MaxCountersSpecification extends Properties("MaxCounters") {
  property("codility example") = Prop {
    MaxCounters.solution(5, Array(3, 4, 4, 6, 1, 4, 4)).toList == List(3, 2, 2, 4, 2)
  }

  property("hazard solutions") = forAll(M, nonMissingGenerator) { (n: Int, a: List[Int]) =>
    MaxCounters.solution(n, a.toArray).toList != List.fill(n)(0)
  }

  property("1000 max counters") = Prop {
    MaxCounters.solution(100, Array.fill(1000)(101)).toList.length == 100
  }

  // move the upper bound
  def N = Gen.choose(1, 100000)
  def M = Gen.choose(1, 1000)

  def nonMissingGenerator: Gen[List[Int]] = for {
    n <- N
    elems <- Gen.listOfN(n, Gen.choose(1, n + 1))
  } yield elems
}