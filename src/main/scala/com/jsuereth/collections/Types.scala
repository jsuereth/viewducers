package com.jsuereth.collections

import scala.collection.mutable

/** A collection of type aliases to clean up other parts of the code. */
object Types {
  /** A left fold.   Gien an accumulator and an element, return the next accumulator.
    *
    * Note: In this library we make no assumption on immutability of the accumulator, so no
    *   save + restore tricks are in place.
    */
  type Fold[Accumulator, -Element] = (Accumulator, Element) => Accumulator


  // TODO - Are we being pedantic about saving classfile size?
  final class BuilderFold[E, To]() extends Fold[mutable.Builder[E, To], E] {
   def apply(acc: mutable.Builder[E, To], e: E): mutable.Builder[E, To] = acc += e
  }
  // This trickery avoids allocations for known pure functions.
  private val dummyAppendFold = new BuilderFold[Nothing,Nothing]()
  def appendFold[E, To] = dummyAppendFold.asInstanceOf[BuilderFold[E,To]]

  final class CountingFold[E] extends Fold[Int, E] {
    def apply(acc: Int, e: E) = acc + 1
  }
  private val dummyCountingFold = new CountingFold[Nothing]
  def countingFold[E] = dummyCountingFold.asInstanceOf[CountingFold[E]]

}

/** This class is used to reduce bytecode footprint.
  *
  * This literally drops ~ 6kb off the library size, and we don't really have that many transducers...
  */
abstract class AbtractFold[Accumulator, -Element] extends Types.Fold[Accumulator, Element]