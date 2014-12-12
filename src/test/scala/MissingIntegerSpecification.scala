package ybr.lesson2

import scala.util.Random

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object MissingIntegerSpecification extends Properties("MissingInteger") {
  property("codility example") = Prop {
    MissingInteger.solution(Array(1, 3, 6, 4, 1, 2)) == 5
  }

  property("negative only") = Prop {
    MissingInteger.solution((-100 to -1).toArray) == 1
  }

  property("large_2") = Prop {
    MissingInteger.solution(Random.shuffle(1 to 100000).toArray) == 100001
  }

  property("good solutions => n + 1") = forAll(nonMissingGenerator) { (a: List[Int]) =>
    MissingInteger.solution(a.toArray) == a.length + 1
  }

  // move the upper bound
  def N = Gen.choose(1, 100)

  def nonMissingGenerator: Gen[List[Int]] = for {
    n <- N
  } yield Random.shuffle(1 to n).toList
}