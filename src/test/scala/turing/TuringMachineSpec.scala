package turing

import common.DistinctByKey
import turing.MachineState.{Accept, Reject}
import turing.TapeSymbol.{Input, LeftEndMarker, RightEndMarker}
import turing.Transition.{FromMiddle, FromRightEnd}
import cats.Eq
import cats.effect.IO
import cats.implicits._
import org.scalacheck.Gen

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.Random

class TuringMachineSpec extends TuringMachineSpecBase {

  "Turing Machines" should {
    "be capable of expressing all grammars" when {
      verifyMachine(
        name    = "Bach sequences (type-1 grammar)",
        machine = BachSequences.machine,
        input   = BachSequences.mixedGenerator
      )(
        function = x => {
          val bySymbol  = x.groupBy(identity)
          val bySetSize = bySymbol.values.groupBy(_.size)
          bySymbol.size == abcCardinality && bySetSize.size == 1
        }
      )

      verifyMachine(
        name    = "palindromes (type-2 grammar)",
        machine = Palindromes.machine,
        input   = Palindromes.mixedGenerator
      )(
        function = x => x.reverse == x
      )

      verifyMachine(
        name    = "alternating sequences (type-3 grammar)",
        machine = AlternatingSequences.machine,
        input   = AlternatingSequences.mixedGenerator
      )(
        function = _.foldLeft(true -> (None: Option[Binary])) { (accum, element) =>
          val (accept, previous) = accum
          (
            accept && !previous.contains_(element),
            Some(element)
          )
        }._1
      )
    }

    /**
      * The search space for input sequences (i.e. the universal set from which a valid input sequence is found) can be
      * large, so it's important our method for finding a valid sequence is faster than O(N) for all machines. Formally,
      * N is `X^Y`, where X is the input alphabet size, and Y is the input sequence size.
      *
      * We verify O(logN) time complexity by running the generator for a machine where N is 839 quadrillion `(62^10)`,
      * and only one valid input sequence exists in the whole set.
      *
      * If it completes within 2 seconds, it cannot be O(N)... so it must be O(logN).
      */
    "generate a valid sequence in O(logN) time, where N is X^Y, X is the input alphabet size, and Y is the input sequence size" in {
      try IO {
        Password.machine.generate(Password.length).head.mkString shouldBe Password.password
      }.unsafeRunTimed(2.seconds)
      catch {
        case e: TimeoutException =>
          throw new Exception("Turing machine sequence generator does not appear to be a O(logN) algorithm!", e)
      }
    }
  }

  /**
    * Hardcoded password (a machine that only matches against one baked-in sequence of input symbols).
    *
    * Represents a machine that is not feasibly linearly searchable from the 'start' node to the 'accept' node, as it
    * has only one valid input sequence in a huge universe of possible input sequences.
    *
    * Specifically designed to:
    * - Not terminate until Right End Marker (i.e. regardless of if we've already determined the input is invalid).
    * - Causes the sequence to keep having new elements appended to it, assuming the generator was implemented using a
    *   naive linear search from the start state, which would cause `62^10` symbols to be generated.
    * - Uses a random password, to avoid accidentally exercising an optimal path in the generator, i.e. if the
    *   generator followed a depth-first search starting from the head transition, and our password was 'aaaaaaaaaa',
    *   then it would generate a valid password on the first iteration.
    */
  object Password {
    val chars: String    = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    val length: Int      = 10
    val password: String = Random.alphanumeric.take(length).toList.mkString

    val machine: Machine[Int, Char, Unit] =
      Machine(
        0,
        DistinctByKey(
          FromRightEnd(password.length, None, accept)
            :: password.zipWithIndex.toList.flatMap {
              case (passwordChar, index) =>
                chars.map { char =>
                  FromMiddle(
                    index,
                    Input(char),
                    noWrite,
                    right,
                    nextState(if (passwordChar == char) index + 1 else -1)
                  )
                }
            } ::: chars.toList.map(char => FromMiddle(-1, Input(char), noWrite, right, noStateChange))
        )
      )

  }

  /**
    * Bach Sequences are a type-1 grammar.
    *
    * They can be expressed by:
    * - Turing Machines
    *
    * Examples (requires each symbol to appear same number of times, in any order):
    * - ABC
    * - ABACCB
    * - ABCABCABC
    *
    * Note:
    * - 3 symbols must be used: apparently 1 is regular, 2 is context-free and 3 is context-sensitive.
    *   See: http://www.cs.ru.nl/~herman/onderwijs/2016TnA/lecture7.pdf
    * - "Bach Sequences" was coined in "Context-freeness and the computer processing of human languages"
    *   (Pullum, Geoffrey K. (1983))
    */
  object BachSequences {
    implicit val eq: Eq[State] = Eq.fromUniversalEquals

    sealed trait State
    case object Start  extends State
    case object Exit   extends State
    case object FindBC extends State
    case object FindAC extends State
    case object FindAB extends State
    case object FindA  extends State
    case object FindB  extends State
    case object FindC  extends State
    case object Back   extends State

    val machine: Machine[State, ABC, Unit] =
      Machine(
        Start,
        DistinctByKey(
          (Start, ())             -> (Start, (), right),           // Find first symbol.
          (Start, A)              -> (FindBC, (), right),          // Erase it.
          (Start, B)              -> (FindAC, (), right),          // ...
          (Start, C)              -> (FindAB, (), right),          // ...
          (Start, RightEndMarker) -> (Exit, RightEndMarker, left), // All symbols erased, so accept (only if non-empty).
          (Exit, ())              -> (Accept, (), hold),           // ...
          (FindBC, A)             -> (FindBC, A, right),           // Find next occurrence of other symbol (1 of 2).
          (FindBC, ())            -> (FindBC, (), right),          // ...
          (FindAC, B)             -> (FindAC, B, right),           // ...
          (FindAC, ())            -> (FindAC, (), right),          // ...
          (FindAB, C)             -> (FindAB, C, right),           // ...
          (FindAB, ())            -> (FindAB, (), right),          // ...
          (FindBC, B)             -> (FindC, (), right),           // Erase it.
          (FindBC, C)             -> (FindB, (), right),           // ...
          (FindAC, A)             -> (FindC, (), right),           // ...
          (FindAC, C)             -> (FindA, (), right),           // ...
          (FindAB, A)             -> (FindB, (), right),           // ...
          (FindAB, B)             -> (FindA, (), right),           // ...
          (FindA, B)              -> (FindA, B, right),            // Find next occurrence of other symbol (2 of 2).
          (FindA, C)              -> (FindA, C, right),            // ...
          (FindA, ())             -> (FindA, (), right),           // ...
          (FindB, A)              -> (FindB, A, right),            // ...
          (FindB, C)              -> (FindB, C, right),            // ...
          (FindB, ())             -> (FindB, (), right),           // ...
          (FindC, A)              -> (FindC, A, right),            // ...
          (FindC, B)              -> (FindC, B, right),            // ...
          (FindC, ())             -> (FindC, (), right),           // ...
          (FindA, A)              -> (Back, (), left),             // Erase it.
          (FindB, B)              -> (Back, (), left),             // ...
          (FindC, C)              -> (Back, (), left),             // ...
          (Back, A)               -> (Back, A, left),              // Go back to start.
          (Back, B)               -> (Back, B, left),              // ...
          (Back, C)               -> (Back, C, left),              // ...
          (Back, ())              -> (Back, (), left),             // ...
          (Back, LeftEndMarker)   -> (Start, LeftEndMarker, right) // ...
        )
      )

    val validGenerator: Gen[List[ABC]] = for {
      subLength <- Gen.choose(0, maxSequenceSize / abcCardinality)
      sorted     = List.fill(subLength)(A) ::: List.fill(subLength)(B) ::: List.fill(subLength)(C)
      shuffled   = Random.shuffle(sorted)
    } yield shuffled

    val mixedGenerator: Gen[List[ABC]] = Gen.oneOf(validGenerator, Gen.listOf(abcGenerator))
  }

  /**
    * Palindromes are a type-2 grammar.
    *
    * They can be expressed by:
    * - Turing Machines
    * - Pushdown Automatons
    *
    * Examples:
    * - 1
    * - 110011
    * - 0010100
    */
  object Palindromes {
    implicit val eq: Eq[State] = Eq.fromUniversalEquals

    sealed trait State
    case object Start     extends State
    case object HaveZero  extends State
    case object HaveOne   extends State
    case object MatchZero extends State
    case object MatchOne  extends State
    case object Back      extends State

    val machine: Machine[State, Binary, Unit] =
      Machine(
        Start,
        DistinctByKey(
          (Start, RightEndMarker)    -> (Accept, RightEndMarker, hold),    // Empty sequence.
          (Start, Zero)              -> (HaveZero, (), right),             // Start of sequence.
          (Start, One)               -> (HaveOne, (), right),              // ...
          (Start, ())                -> (Accept, (), hold),                // Starting again, but nothing left to process.
          (HaveZero, One)            -> (HaveZero, One, right),            // Skip over other symbols, until we reach the end.
          (HaveZero, Zero)           -> (HaveZero, Zero, right),           // ...
          (HaveOne, One)             -> (HaveOne, One, right),             // ...
          (HaveOne, Zero)            -> (HaveOne, Zero, right),            // ...
          (HaveZero, ())             -> (MatchZero, (), left),             // Reached the end.
          (HaveZero, RightEndMarker) -> (MatchZero, RightEndMarker, left), // ...
          (HaveOne, ())              -> (MatchOne, (), left),              // ...
          (HaveOne, RightEndMarker)  -> (MatchOne, RightEndMarker, left),  // ...
          (MatchZero, Zero)          -> (Back, (), left),                  // Verify last character matches first character.
          (MatchZero, One)           -> (Reject, One, hold),               // ...
          (MatchZero, ())            -> (Accept, (), hold),                // ...
          (MatchOne, One)            -> (Back, (), left),                  // ...
          (MatchOne, Zero)           -> (Reject, Zero, hold),              // ...
          (MatchOne, ())             -> (Accept, (), hold),                // ...
          (Back, Zero)               -> (Back, Zero, left),                // Go back to start.
          (Back, One)                -> (Back, One, left),                 // ...
          (Back, ())                 -> (Start, (), right)                 // ...
        )
      )

    val validGenerator: Gen[List[Binary]] = for {
      length <- Gen.choose(0, maxSequenceSize)
      base   <- Gen.listOfN((length - 1) / 2, binaryGenerator)
      middle <- Gen.option(binaryGenerator)
    } yield base ::: middle.toList ::: base.reverse

    val mixedGenerator: Gen[List[Binary]] = Gen.oneOf(validGenerator, Gen.listOf(binaryGenerator))
  }

  /**
    * Alternating sequences are a type-3 grammar.
    *
    * They can be expressed by:
    * - Turing Machines
    * - Pushdown Automatons
    * - Finite State Machines (FSMs)
    *
    * Examples:
    * - 1
    * - 1010
    * - 01010
    */
  object AlternatingSequences {

    val machine: Machine[Option[Binary], Binary, Unit] =
      Machine(
        None,
        DistinctByKey(
          (None, RightEndMarker)      -> (Accept, RightEndMarker, hold), // Empty sequence.
          (None, Zero)                -> (Zero.some, Zero, right),       // Start of sequence.
          (None, One)                 -> (One.some, One, right),         // ...
          (Zero.some, One)            -> (One.some, One, right),         // Middle of sequence.
          (One.some, Zero)            -> (Zero.some, Zero, right),       // ...
          (Zero.some, RightEndMarker) -> (Accept, RightEndMarker, hold), // End of non-empty sequence.
          (One.some, RightEndMarker)  -> (Accept, RightEndMarker, hold)  // ...
        )
      )

    val validGenerator: Gen[List[Binary]] =
      for {
        seed   <- binaryGenerator
        length <- Gen.choose(0, maxSequenceSize)
      } yield (0 until length).toList.map { x =>
        if (x % 2 == 0)
          seed
        else
          seed.flip
      }

    val mixedGenerator: Gen[List[Binary]] = Gen.oneOf(validGenerator, Gen.listOf(binaryGenerator))

  }

}
