package turing

import common.{CacheUtils, DistinctByKey}
import turing.TapeSymbol.{LeftEndMarker, RightEndMarker}
import cats.Eq
import cats.implicits._

/**
  * Turing Machine (technically a "Linear Bounded Automaton").
  *
  * Follows the Minsky 1967 definition:
  * - Each transition can both print a symbol and then optionally move the head L/R.
  * - Does not use "The Spurious Turing Convention" (i.e. F-Squares and E-Squares).
  *
  * Further, we introduce our own conventions:
  * - Tape size is finite and cannot grow beyond its initial size (aka a linear bounded automaton).
  * - Left/Right end markers are inserted before and after the input on the tape.
  *   - You cannot overwrite these markers.
  *   - You cannot move the head beyond these markers.
  * - Terminal states are hardcoded to 'Accept' and 'Reject'.
  * - If a transition doesn't exist for a state/symbol combination, we implicitly transition to 'Reject'.
  */
case class Machine[S: Eq, I, O](
  start: S,
  transitions: DistinctByKey[Transition[S, I, O]]
) {

  def parse(input: List[I]): Boolean =
    MachineConfiguration
      .forParsing(machine = this, input = input)
      .parse
      .accepted

  def generate(maxSequenceLength: Int): LazyList[List[I]] =
    MachineConfiguration
      .forGenerating(machine = this)
      .generate(maxSequenceLength)

  def getTransitionsTo(
    state: MachineState[S],
    left: Option[TapeSymbol[I, O]],
    head: Option[TapeSymbol[I, O]],
    right: Option[TapeSymbol[I, O]]
  ): List[Transition[S, I, O]] =
    getTransitionsToCached(state, left, head, right)

  def getTransitionsTo(
    state: MachineState[S],
    includeLeftMovements: Boolean,
    includeRightMovements: Boolean,
    limitLeftMovementWrites: Option[TapeSymbol[I, O]],
    limitRightMovementWrites: Option[TapeSymbol[I, O]],
    limitStationaryWrites: Option[TapeSymbol[I, O]]
  ): List[Transition[S, I, O]] =
    transitionsTo.get(state) match {
      case Some(byMovement) =>
        getTransitionsBySymbolMaybe(
          includeRightMovements,
          limitRightMovementWrites,
          byMovement.right
        ) ::: getTransitionsBySymbolMaybe(
          includeLeftMovements,
          limitLeftMovementWrites,
          byMovement.left
        ) ::: getTransitionsBySymbol(limitStationaryWrites, byMovement.none)
      case None => Nil
    }

  private def getTransitionsBySymbolMaybe(
    include: Boolean,
    limit: Option[TapeSymbol[I, O]],
    bySymbol: BySymbol[S, I, O]
  ): List[Transition[S, I, O]] =
    if (include)
      getTransitionsBySymbol(limit, bySymbol)
    else
      Nil

  private def getTransitionsBySymbol(
    limit: Option[TapeSymbol[I, O]],
    bySymbol: BySymbol[S, I, O]
  ): List[Transition[S, I, O]] =
    limit.fold(bySymbol.all)(bySymbol.grouped.getOrElse(_, Nil))

  private lazy val transitionsTo: Map[MachineState[S], ByMovement[S, I, O]] =
    transitions.value.values.groupBy(_.nextState).view.mapValues { transitionsByState =>
      def bySymbol(transitions: Iterable[Transition[S, I, O]]): BySymbol[S, I, O] =
        BySymbol(
          transitions.groupBy(_.to.leave).view.mapValues(_.toList).toMap,
          transitions.toList
        )
      val transitionsByMovement = transitionsByState.groupBy(_.move)
      val none                  = transitionsByMovement.getOrElse(None, Nil)
      val left                  = transitionsByMovement.getOrElse(().asLeft.some, Nil)
      val right                 = transitionsByMovement.getOrElse(().asRight.some, Nil)
      ByMovement(
        none  = bySymbol(none),
        left  = bySymbol(left),
        right = bySymbol(right)
      )
    }.toMap

  // Poor man's caching: think of this 'val' as a 'def'.
  private val getTransitionsToCached: (
    (MachineState[S], Option[TapeSymbol[I, O]], Option[TapeSymbol[I, O]], Option[TapeSymbol[I, O]])
  ) => List[Transition[S, I, O]] = CacheUtils.memoize {
    case (state, left, head, right) =>
      getTransitionsTo(
        state = state,
        // Intentionally swap left/right here: when a previous transition moves right, it means
        // it wrote to the left of the future head (i.e. current head) and visa-versa.
        includeLeftMovements     = !head.contains_(RightEndMarker),
        includeRightMovements    = !head.contains_(LeftEndMarker),
        limitLeftMovementWrites  = right,
        limitRightMovementWrites = left,
        limitStationaryWrites    = head
      )
  }

}

case class ByMovement[S, I, O](
  none: BySymbol[S, I, O],
  left: BySymbol[S, I, O],
  right: BySymbol[S, I, O]
)

case class BySymbol[S, I, O](
  grouped: Map[TapeSymbol[I, O], List[Transition[S, I, O]]],
  all: List[Transition[S, I, O]]
)
