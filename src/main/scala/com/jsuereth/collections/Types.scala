package com.jsuereth.collections

/** A collection of type aliases to clean up other parts of the code. */
object Types {
  /** A left fold.   Gien an accumulator and an element, return the next accumulator.
    *
    * Note: In this library we make no assumption on immutability of the accumulator, so no
    *   save + restore tricks are in place.
    */
  type Fold[Accumulator, -Element] = (Accumulator, Element) => Accumulator
}