package ybr

import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary._

object PermCheckSpecification extends Properties("PermCheck") {
  property("solution") = forAll(permGenerator) { (a: List[Int]) =>
    PermCheck.solution(a.toArray) == 1
  }

  property("solution hazard") = forAll(notPermGenerator) { (a: List[Int]) =>
    PermCheck.solution(a.toArray) == 0
  }

  // move the upper bound
  def size = Gen.choose(1, 4)
  // move the upper bound
  def element = Gen.choose(1, 4)

  // generates a permutation of the n first integers
  def permGenerator: Gen[List[Int]] = for {
    n <- size
  } yield scala.util.Random.shuffle(1 to n).toList

  // add some problematic noise to a generated permutation
  def notPermGenerator = for {
    perm <- permGenerator
    m <- size
    random <- Gen.listOfN(m, element suchThat (_ != perm.length + 1))
  } yield scala.util.Random.shuffle(perm ++ random)
}