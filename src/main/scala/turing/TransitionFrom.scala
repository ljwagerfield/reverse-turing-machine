package turing

case class TransitionFrom[+S, +I, +O](current: S, read: TapeSymbol[I, O])
