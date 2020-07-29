package turing

/**
  * Turing Machine tape with head position.
  *
  * Tapes can be bounded or unbounded.
  *
  * All operations are O(1) except for 'toList' which is O(N).
  */
sealed trait Tape[R, W <: R] extends Product {
  def size: Int
  def toList: List[W]

  def headWritable: Option[W]
  def leftWritable: Option[W]
  def rightWritable: Option[W]

  def write(value: W): Tape[R, W]
  def moveLeft: Tape[R, W]
  def moveRight: Tape[R, W]
  def bindLeft: Tape[R, W]
  def bindRight: Tape[R, W]

  def isLeftBounded: Boolean
  def isRightBounded: Boolean
  def leftMarker: R
  def rightMarker: R
  protected def leftMarkerIfBounded: Option[R]  = if (isLeftBounded) Some(leftMarker) else None
  protected def rightMarkerIfBounded: Option[R] = if (isRightBounded) Some(rightMarker) else None

  def head: Option[R]
  def left: Option[R]  = if (isLeftBounded) leftWritable.orElse(leftMarkerIfBounded) else leftWritable
  def right: Option[R] = if (isRightBounded) rightWritable.orElse(rightMarkerIfBounded) else rightWritable

  def atRightEnd: Boolean = head.contains(rightMarker)
  def atLeftEnd: Boolean  = head.contains(leftMarker)
}

object Tape {

  def bounded[R, W <: R](list: List[W], leftMarker: R, rightMarker: R): Tape[R, W] =
    apply(list, isLeftBounded = true, isRightBounded = true, leftMarker, rightMarker)

  def unbounded[R, W <: R](list: List[W], leftMarker: R, rightMarker: R): Tape[R, W] =
    apply(list, isLeftBounded = false, isRightBounded = false, leftMarker, rightMarker)

  private def apply[R, W <: R](
    item: W,
    isLeftBounded: Boolean,
    isRightBounded: Boolean,
    leftMarker: R,
    rightMarker: R
  ): Tape[R, W] =
    NonEmptyTape(1, Some(item), Nil, Nil, isLeftBounded, isRightBounded, leftMarker, rightMarker)

  private def apply[R, W <: R](
    list: List[W],
    isLeftBounded: Boolean,
    isRightBounded: Boolean,
    leftMarker: R,
    rightMarker: R
  ): Tape[R, W] =
    if (list.isEmpty)
      EmptyTape(headAtRightEnd = true, isLeftBounded, isRightBounded, leftMarker, rightMarker)
    else
      NonEmptyTape(list.size, list.headOption, Nil, list.tail, isLeftBounded, isRightBounded, leftMarker, rightMarker)

  case class EmptyTape[R, W <: R](
    headAtRightEnd: Boolean,
    isLeftBounded: Boolean,
    isRightBounded: Boolean,
    leftMarker: R,
    rightMarker: R
  ) extends Tape[R, W] {
    def size: Int                   = 0
    def toList: List[W]             = Nil
    def headWritable: Option[W]     = None
    def leftWritable: Option[W]     = None
    def rightWritable: Option[W]    = None
    def write(value: W): Tape[R, W] = Tape(value, isLeftBounded, isRightBounded, leftMarker, rightMarker)
    def bindLeft: Tape[R, W]        = copy(isLeftBounded  = true)
    def bindRight: Tape[R, W]       = copy(isRightBounded = true)
    def moveLeft: Tape[R, W]        = copy(headAtRightEnd = false)
    def moveRight: Tape[R, W]       = copy(headAtRightEnd = true)
    def head: Option[R]             = if (headAtRightEnd) rightMarkerIfBounded else leftMarkerIfBounded
  }

  case class NonEmptyTape[R, W <: R](
    size: Int,
    headWritable: Option[W],
    prefix: List[W],
    suffix: List[W],
    isLeftBounded: Boolean,
    isRightBounded: Boolean,
    leftMarker: R,
    rightMarker: R
  ) extends Tape[R, W] {
    def toList: List[W]          = prefix.reverse ::: headWritable.toList ::: suffix
    def leftWritable: Option[W]  = prefix.headOption
    def rightWritable: Option[W] = suffix.headOption
    def bindLeft: Tape[R, W]     = copy(isLeftBounded  = true)
    def bindRight: Tape[R, W]    = copy(isRightBounded = true)

    def write(value: W): Tape[R, W] =
      copy(
        headWritable = Some(value),
        size         = if (headWritable.isDefined) size else size + 1
      )

    def moveLeft: Tape[R, W] =
      copy(
        headWritable = prefix.headOption,
        prefix       = if (prefix.isEmpty) Nil else prefix.tail,
        suffix       = headWritable.fold(suffix)(_ :: suffix)
      )

    def moveRight: Tape[R, W] =
      copy(
        headWritable = suffix.headOption,
        prefix       = headWritable.fold(prefix)(_ :: prefix),
        suffix       = if (suffix.isEmpty) Nil else suffix.tail
      )

    def head: Option[R] =
      headWritable.orElse {
        if (suffix.isEmpty)
          rightMarkerIfBounded
        else
          leftMarkerIfBounded
      }

  }

}
