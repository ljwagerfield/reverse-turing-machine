package turing

import cats.Eq

sealed trait TapeSymbol[+I, +O] extends Product

object TapeSymbol {
  implicit def eq[I, O]: Eq[TapeSymbol[I, O]] = Eq.fromUniversalEquals
  implicit val eq2: Eq[EndMarker]             = Eq.fromUniversalEquals

  sealed trait IOSymbol[+I, +O] extends TapeSymbol[I, O]

  case class Input[I](value: I) extends IOSymbol[I, Nothing] {
    override def hashCode(): Int = value.hashCode()
  }

  case class Output[O](value: O) extends IOSymbol[Nothing, O] {
    override def hashCode(): Int = value.hashCode()
  }

  sealed trait EndMarker     extends TapeSymbol[Nothing, Nothing]
  case object LeftEndMarker  extends EndMarker
  case object RightEndMarker extends EndMarker
}
