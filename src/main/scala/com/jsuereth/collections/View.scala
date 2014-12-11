package com.jsuereth.collections

import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom


abstract class View[E, To] {
  type From
  type SourceElement

  val cbf: CanBuildFrom[From, E, To]

  /** The source collection.  TODO - can we leave this unbound? */
  val underlying: StagedCollectionOps[E]

  // Forces the collection into a result
  def force: To = {
    // TODO - some type hackery to get this to work.
    underlying.to_![GenTraversable](cbf.asInstanceOf[CanBuildFrom[Nothing,E, GenTraversable[E]]]).asInstanceOf[To]
  }

  def map[B, NextTo](f: E => B)(implicit cbf: CanBuildFrom[To, B, NextTo]): View[B, NextTo] =
    SimpleView(
      underlying.map(f),
      cbf.asInstanceOf[CanBuildFrom[From, B, NextTo]]
    )
}

private[collections] case class SimpleView[Orig, E, To](
  underlying: StagedCollectionOps[E],
  cbf: CanBuildFrom[Orig, E, To]
) extends View[E, To] {
  type SourceElement = E
  type From = Orig
}


object View {

  import scala.collection.GenTraversableLike
  import scala.collection.generic.IsTraversableLike
  class ExtensionMethods[A, Repr](underlying: StagedCollectionOps[A]) {
    def stagedView(implicit cbf: CanBuildFrom[Repr, A, Repr]): View[A, Repr] = SimpleView[Repr, A, Repr](underlying, cbf)
  }
  implicit def withExtensions[Repr](coll: Repr)(implicit traversable: IsTraversableLike[Repr]) =
    new ExtensionMethods[traversable.A, Repr](
      StagedCollectionOps[traversable.A](traversable.conversion(coll)))
}
