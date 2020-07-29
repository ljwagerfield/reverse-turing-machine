package turing

case class TransitionTo[+S, +I, +O](leave: TapeSymbol[I, O], move: Option[Either[Unit, Unit]], next: MachineState[S])
