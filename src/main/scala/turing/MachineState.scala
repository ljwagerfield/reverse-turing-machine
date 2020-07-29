package turing

import turing.MachineState.{Accept, NonTerminalState, Reject}
import cats.Eq

sealed trait MachineState[+S] {

  def accepted: Boolean =
    this match {
      case Accept                          => true
      case Reject | _: NonTerminalState[_] => false
    }

}

object MachineState {
  implicit def eq[S]: Eq[MachineState[S]] = Eq.fromUniversalEquals

  sealed trait TerminalState extends MachineState[Nothing] {

    def fold[A](accept: => A, reject: => A): A =
      this match {
        case Accept => accept
        case Reject => reject
      }

  }

  case object Accept extends TerminalState
  case object Reject extends TerminalState

  case class NonTerminalState[+S](value: S) extends MachineState[S] {
    override def hashCode(): Int = value.hashCode()
  }

}
