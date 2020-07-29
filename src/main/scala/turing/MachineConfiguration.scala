package turing

import common.TreeUtils
import turing.MachineState.{Accept, NonTerminalState, Reject, TerminalState}
import turing.TapeSymbol.{IOSymbol, Input, LeftEndMarker, Output, RightEndMarker}
import cats.implicits._
import cats.{Eq, Id}

case class MachineConfiguration[S: Eq, I, O](
  machine: Machine[S, I, O],
  state: MachineState[S],
  tape: Tape[TapeSymbol[I, O], IOSymbol[I, O]]
) {

  /** Generates valid input sequences for the Turing Machine by playing it in reverse.
    *
    * @param maxTapeLength This parameter limits the depth of the search, forcing the internal DFS algorithm to
    *                      exhaustively cover a finite area, rather than infinitely traversing down one side of the
    *                      tree of possible machine configurations for valid sequences.
    *
    *                      IDDFS is not used since it requires re-traversal of nodes, and in this tree, children
    *                      are generated on-the-fly, meaning IDDFS results in lots of extra turing machine cycles.
    *
    *                      BFS, whilst having a desirable "ordered by length ascending" output, is too memory
    *                      intensive. We have therefore chosen DFS, as it's the most space and time optimal.
    *
    *                      DFS also provides some extra flexibility:
    *
    *                      1) if the caller chooses a small enough depth, with the intention of consuming the entire
    *                         output, the set of sequences returned will be the same as that from a BFS for the same
    *                         number of sequences (albeit in a different order). Thus it will be "like" using a BFS.
    *
    *                      2) if the machine's valid sequences exist in higher density in 'vertical wedges' taken from
    *                         the tree of all possible configurations, compared to same-size 'top sections' of the tree,
    *                         then the DFS will in fact yield valid sequences at a higher rate than a BFS, in which case
    *                         it will be desirable for the user to specify a large tape length, and let the DFS go deep.
    */
  def generate(maxTapeLength: Int): LazyList[List[I]] =
    TreeUtils
      .dfs(this)(_.previousConfigurations(maxTapeLength))
      .flatMap(_.initialTape)

  private def previousConfigurations(maxTapeLength: Int): IterableOnce[MachineConfiguration[S, I, O]] = {
    val validPreviousTransitions = machine.getTransitionsTo(
      state = state,
      left  = tape.left,
      head  = tape.head,
      right = tape.right
    )

    validPreviousTransitions.iterator.map { transition =>
      val moved = transition.to.move.fold(tape)(_.fold(_ => tape.moveRight, _ => tape.moveLeft))
      copy(
        state = NonTerminalState(transition.current),
        tape = transition.read match {
          case LeftEndMarker          => moved.bindLeft
          case RightEndMarker         => moved.bindRight
          case symbol: IOSymbol[I, O] => moved.write(symbol)
        }
      )
    }.filter(_.tape.size <= maxTapeLength)
  }

  private def initialTape: Option[List[I]] =
    if (state === NonTerminalState(machine.start) && tape.leftWritable.isEmpty) {
      val tapeList  = tape.toList
      val inputs    = tapeList.collect { case Input(input) => input }
      val allInputs = inputs.size === tapeList.size
      Some(inputs).filter(_ => allInputs).map(_.toList)
    }
    else
      None

  /**
    * Validates the current [[tape]] against the machine's rules.
    *
    * @return [[Accept]] if the tape follows the [[machine]]'s rules, otherwise [[Reject]].
    */
  def parse: TerminalState =
    this.tailRecM[Id, TerminalState](_.parseNext)

  private def parseNext: Either[MachineConfiguration[S, I, O], TerminalState] =
    state match {
      case s: TerminalState =>
        s.asRight
      case NonTerminalState(s) =>
        val transitionFrom      = TransitionFrom[S, I, O](s, tape.head.getOrElse(throw new Exception("Tape is unbounded.")))
        val transitionToDefault = TransitionTo[S, I, O](transitionFrom.read, None, Reject)
        val transitionTo        = machine.transitions.value.get(transitionFrom).fold(transitionToDefault)(_.to)
        val updatedSymbol =
          transitionTo.leave
            .some
            .filter(_ =!= transitionFrom.read)
            .collect { case x: Output[O] => x }
        val written         = updatedSymbol.fold(tape)(tape.write)
        val writtenAndMoved = transitionTo.move.fold(written)(_.fold(_ => written.moveLeft, _ => written.moveRight))

        copy(
          state = transitionTo.next,
          tape  = writtenAndMoved
        ).asLeft
    }

}

object MachineConfiguration {

  def forParsing[S: Eq, I, O](machine: Machine[S, I, O], input: List[I]): MachineConfiguration[S, I, O] =
    MachineConfiguration(
      machine = machine,
      state   = NonTerminalState(machine.start),
      tape    = Tape.bounded(input.map(Input.apply), LeftEndMarker, RightEndMarker)
    )

  def forGenerating[S: Eq, I, O](machine: Machine[S, I, O]): MachineConfiguration[S, I, O] =
    MachineConfiguration(
      machine = machine,
      state   = Accept,
      tape    = Tape.unbounded(Nil, LeftEndMarker, RightEndMarker)
    )

}
