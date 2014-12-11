package com.jsuereth.collections

/** A collection of type aliases to clean up other parts of the code. */
object Types {
  /** A left fold **/
  type Fold[Accumulator, -Element] = (Accumulator, Element) => Accumulator
}