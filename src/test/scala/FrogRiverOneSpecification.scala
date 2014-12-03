package ybr

import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object FrogRiverOneSpecification extends Properties("FrogRiverOne") {
  property("codility example") = Prop {
    FrogRiverOne.solution(5, Array(1, 3, 1, 4, 2, 3, 5, 4)) == 6
  }

  property("path exists") = forAll(X, pathGenerator) { (x: Int, a: List[Int]) =>
    FrogRiverOne.solution(x, a.toArray) != -1 ==> (a.length > 0)
  }

  property("path does not exist") = forAll(X, noPathGenerator) { (x: Int, a: List[Int]) =>
    FrogRiverOne.solution(x, a.toArray) == -1 ==> (a.length > 0)
  }

  // move the upper bound
  def N = Gen.choose(1, 10)
  def X = Gen.choose(1, 10)
  def elem = Gen.choose(1, 10)

  def pathGenerator: Gen[List[Int]] = for {
    n <- N
    m <- N
    random <- Gen.listOfN(m, elem)
  } yield scala.util.Random.shuffle((1 to n).toList ++ random)

  def noPathGenerator: Gen[List[Int]] = for {
    path <- pathGenerator
    missingLeaf <- X
  } yield path.filter(_ != missingLeaf)
}