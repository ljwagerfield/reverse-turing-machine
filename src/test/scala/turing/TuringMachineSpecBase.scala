package turing

import cats.Eq
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers

abstract class TuringMachineSpecBase extends AnyWordSpec with Matchers with Checkers {
  implicit val eq: Eq[ABC]             = Eq.fromUniversalEquals
  implicit val eq2: Eq[Binary]         = Eq.fromUniversalEquals
  implicit val eq3: Eq[Option[Binary]] = Eq.fromUniversalEquals
  val abcCardinality                   = 3
  val abcGenerator: Gen[ABC]           = Gen.oneOf(A, B, C)
  val binaryGenerator: Gen[Binary]     = Gen.oneOf(Zero, One)
  val maxSequenceSize                  = 9

  sealed trait Binary extends Product {
    def flip: Binary
  }

  case object One  extends Binary { def flip: Binary = Zero }
  case object Zero extends Binary { def flip: Binary = One  }

  sealed trait ABC extends Product
  case object A    extends ABC
  case object B    extends ABC
  case object C    extends ABC

  def verifyMachine[S, I, O](name: String, machine: Machine[S, I, O], input: Gen[List[I]])(
    function: List[I] => Boolean
  ): Unit = {
    s"parsing $name" in {
      check(forAll(input) { s =>
        val expected = function(s)
        val actual   = machine.parse(s)
        expected == actual
      })
    }

    s"generating $name" in {
      val validSequences = machine.generate(maxSequenceSize).toList

      check(forAll(input.filter(_.size <= maxSequenceSize)) { s =>
        val expected = function(s)
        val actual   = validSequences.contains(s)
        expected == actual
      })
    }
  }

}
