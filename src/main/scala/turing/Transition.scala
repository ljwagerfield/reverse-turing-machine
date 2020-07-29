package turing

import common.HasKey
import turing.MachineState.NonTerminalState
import turing.TapeSymbol.{IOSymbol, LeftEndMarker, Output, RightEndMarker}

sealed trait Transition[+S, +I, +O] extends Product {
  def current: S
  def read: TapeSymbol[I, O]
  def write: Option[O]
  def move: Option[Either[Unit, Unit]]
  def changeState: Option[MachineState[S]]
  val nextState: MachineState[S]    = changeState.getOrElse(NonTerminalState(current))
  val from: TransitionFrom[S, I, O] = TransitionFrom(current, read)
  val to: TransitionTo[S, I, O]     = TransitionTo(write.map(Output.apply).getOrElse(read), move, nextState)
}

object Transition {

  implicit def hasKey[S, I, O]: HasKey[TransitionFrom[S, I, O], Transition[S, I, O]] =
    new HasKey[TransitionFrom[S, I, O], Transition[S, I, O]] {
      override def key: Transition[S, I, O] => TransitionFrom[S, I, O] = _.from
    }

  case class FromMiddle[+S, I, O](
    current: S,
    read: IOSymbol[I, O],
    write: Option[O],
    move: Option[Either[Unit, Unit]],
    changeState: Option[MachineState[S]]
  ) extends Transition[S, I, O]

  case class FromLeftEnd[S, I, O](current: S, move: Option[Right[Unit, Unit]], changeState: Option[MachineState[S]])
    extends Transition[S, I, O] {
    override def read: TapeSymbol[I, O] = LeftEndMarker
    override def write: Option[O]       = None
  }

  case class FromRightEnd[S, I, O](current: S, move: Option[Left[Unit, Unit]], changeState: Option[MachineState[S]])
    extends Transition[S, I, O] {
    override def read: TapeSymbol[I, O] = RightEndMarker
    override def write: Option[O]       = None
  }

}
