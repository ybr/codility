package ybr.lesson3

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object PassingCarsSpecification extends Properties("PassingCars") {
  property("codility example") = Prop {
    PassingCars.solution(Array(0, 1, 0, 1, 1)) == 5
  }

  property("very high count") = Prop {
    PassingCars.solution(Array.fill(50000)(0) ++ Array.fill(50000)(1)) == -1
  }

  property("hazard") = forAll(passingCarsGenerator) { (a: List[Int]) =>
    val partition = a.partition(_ == 0)
    PassingCars.solution(a.toArray) >= 0
  }

  // // move the upper bound
  def N = Gen.choose(1, 100)

  def passingCarsGenerator: Gen[List[Int]] = for {
    n <- N
    elems <- Gen.listOfN(n, Gen.choose(0, 1))
  } yield elems
}