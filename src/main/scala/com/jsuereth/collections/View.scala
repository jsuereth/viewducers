package com.jsuereth.collections

import scala.collection.{GenTraversableOnce, GenTraversable}
import scala.collection.generic.CanBuildFrom


/** A view represents a set of staged collection operations against an original collection.
  * Additionally, a view attempts to keep track of the most optimal collection to return from the
  * series of operations, using the `CanBuildFrom` pattern.
  *
  * @tparam E  The elements in the collection.
  * @tparam To The resulting collection type of the staged operations acrued so far.
  */
abstract class View[E, To] {
  /** The underlying implementation of the original collection.
    *
    * We need this type to have existed at one time, but we no longer care what it is.
    */
  protected type From

  /** The currently saved builder we will use to build the result of the staged operations. */
  protected val cbf: CanBuildFrom[From, E, To]

  /** The source collection.  TODO - can we leave this unbound? */
  val underlying: StagedCollectionOps[E]

  /** Forces the staged operations to return the new collection. */
  final def force: To = {
    // TODO - some type hackery to get this to work.
    underlying.to_![GenTraversable](cbf.asInstanceOf[CanBuildFrom[Nothing,E, GenTraversable[E]]]).asInstanceOf[To]
  }


  // TODO - Docment the standard methods.
  final def map[B, NextTo](f: E => B)(implicit cbf: CanBuildFrom[To, B, NextTo]): View[B, NextTo] =
    SimpleView(
      underlying.map(f),
      cbf.asInstanceOf[CanBuildFrom[From, B, NextTo]]
    )
  final def filter(f: E => Boolean): View[E, To] = SimpleView(underlying.filter(f), cbf)
  final def zipWithIndex[NextTo](implicit nextCbf: CanBuildFrom[To, (E, Int), NextTo]): View[(E, Int), NextTo] =
    SimpleView(underlying.zipWithIndex, nextCbf)
  final def flatMap[B, NextTo](f: E => GenTraversableOnce[B])(implicit nextCbf: CanBuildFrom[To,B, NextTo]): View[B, NextTo] =
    SimpleView(underlying.flatMap(f), nextCbf)
  final def slice(start: Int, end: Int): View[E,To] = SimpleView(underlying.slice(start, end), cbf)
  final def take(n: Int): View[E,To] = SimpleView(underlying.take(n), cbf)
  final def drop(n: Int): View[E,To] = SimpleView(underlying.drop(n), cbf)
}

/** Simple implementation of view with no fancy frills. */
private[collections] case class SimpleView[Orig, E, To](
  underlying: StagedCollectionOps[E],
  cbf: CanBuildFrom[Orig, E, To]
) extends View[E, To] {
  type From = Orig
}


object View {
  import scala.collection.generic.IsTraversableLike
  /** This is our set of extension methods for existing collections. */
  final class ExtensionMethods[A, Repr](val staged: StagedCollectionOps[A]) extends AnyVal {
    def stagedView(implicit cbf: CanBuildFrom[Repr, A, Repr]): View[A, Repr] = SimpleView[Repr, A, Repr](staged, cbf)
  }
  implicit def withExtensions[Repr](coll: Repr)(implicit traversable: IsTraversableLike[Repr]) =
    new ExtensionMethods[traversable.A, Repr](
      StagedCollectionOps[traversable.A](traversable.conversion(coll)))
}
