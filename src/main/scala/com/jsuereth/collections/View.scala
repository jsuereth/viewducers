package com.jsuereth.collections

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.{GenTraversableOnce, GenTraversable}
import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions


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
    // Some type hackery to get this to work.  The beauty is, we've verified all the types on the input,
    // and we know how we use the builder, so these casts are ok optimisations.
    underlying.to_![GenTraversable](cbf.asInstanceOf[CanBuildFrom[Nothing,E, GenTraversable[E]]]).asInstanceOf[To]
  }

  /** This will force the underyling operations to run, returning a new view against the temporary collection. */
  final def memoize(implicit toGenTraversable: To => GenTraversable[E]): View[E,To] =
    if(underlying.hasStagedOperations) SimpleView(StagedCollectionOps(toGenTraversable(force)), cbf)
    else this

  // TODO - we should probably keep some sort of atomic 'var' which contains a memoized instance of this view.
  //        The memoize function can simple return "this" after memoizing, and we can ensure this is called from
  //        any terminal operation.
  //        Additionally, we can base any further view chains off this memoized view rather than repeating the work again.
  // One issue here is we need to know that "To" can actually be traversed.  It's possible it could be something like
  // "Array" or "String" where we need to run a conversion to get the GenTraversableOnce interface we need.
  // We could look into capturing that in all terminal operations (like force).




  // TODO - Document the standard methods.
  final def map[B, NextTo](f: E => B)(implicit cbf: CanBuildFrom[To, B, NextTo]): View[B, NextTo] =
    SimpleView(
      underlying.map(f),
      cbf.asInstanceOf[CanBuildFrom[From, B, NextTo]]
    )
  final def collect[B, NextTo](f: PartialFunction[E,B])(implicit cbf: CanBuildFrom[To, B, NextTo]): View[B, NextTo] =
    SimpleView(
      underlying.collect(f),
      cbf.asInstanceOf[CanBuildFrom[From, B, NextTo]]
    )
  final def filter(f: E => Boolean): View[E, To] = SimpleView(underlying.filter(f), cbf)
  final def zipWithIndex[NextTo](implicit nextCbf: CanBuildFrom[To, (E, Int), NextTo]): View[(E, Int), NextTo] =
    SimpleView(underlying.zipWithIndex, nextCbf)
  final def flatMap[B, NextTo](f: E => GenTraversableOnce[B])(implicit nextCbf: CanBuildFrom[To,B, NextTo]): View[B, NextTo] =
    SimpleView(underlying.flatMap(f), nextCbf)
  final def flatten[B, NextTo](implicit asTraversable: (E) ⇒ GenTraversableOnce[B], nextCbf: CanBuildFrom[To,B, NextTo]): View[B, NextTo] =
    flatMap(asTraversable)(nextCbf)
  final def slice(start: Int, end: Int): View[E,To] = SimpleView(underlying.slice(start, end), cbf)
  final def take(n: Int): View[E,To] = SimpleView(underlying.take(n), cbf)
  final def drop(n: Int): View[E,To] = SimpleView(underlying.drop(n), cbf)
  final def takeWhile(f: E => Boolean): View[E,To] = SimpleView(underlying.takeWhile(f), cbf)
  final def dropWhile(f: E => Boolean): View[E,To]   = SimpleView(underlying.dropWhile(f), cbf)


  // Terminal methods


  // TODO - this should return a view....
  final def ++[B >: E, That](xs: GenTraversableOnce[B])(implicit bf: CanBuildFrom[To, B, That]): That = {
    val builder = bf()
    // TODO use anonymous fold class...
    underlying.foldLeft_!(builder)(Types.appendFold)
    xs.foldLeft(builder)(Types.appendFold)
    // SO, in new fun news, we need to return a VIEW here, yeah...
    builder.result()
  }
  final def count(p: E => Boolean): Int = {
    // TODO - We are using mutability and assumed sequential behavior because boxing/unboxing is actually a burden on this computation.
    var acc = 0
    underlying.foldLeft_!(null) { (ignore, el) =>
      if (p(el)) acc += 1
      null
    }
    acc
  }
  final def exists(p: E => Boolean): Boolean = find(p).isDefined
  final def find(p: E => Boolean): Option[E] =
    underlying.foldLeft_!(Option.empty[E]) {(acc, el) =>
      if(acc.isEmpty && p(el)) Transducer.earlyExit(Some(el))
      else acc
    }
  final def fold[A1 >: E](z: A1)(op: (A1, A1) ⇒ A1): A1 = underlying.foldLeft_!(z)(op)
  final def foldLeft[Accumulator](z: Accumulator)(op: (Accumulator, E) ⇒ Accumulator): Accumulator = underlying.foldLeft_!(z)(op)
  // TODO - less bytecode heavy mechanism here.
  final def forall(p: E => Boolean): Boolean = {
    val not = (e: E) => !p(e)
    !exists(not)
  }
  // TODO - groupBy
  final def head: E = headOption.getOrElse(throw new IllegalStateException("Cannot call head on an empty collection/view!"))
  final def headOption: Option[E] =
      underlying.foldLeft_!(Option.empty[E]) { (acc, el) =>
        if(acc.isEmpty) Transducer.earlyExit(Some(el))
        else acc
      }
  // TODO - init
  // TDOO - inits
  // TODO - isEmpty
  // TODO - isTraversableAgain

  final def size: Int = underlying.size_!
  final def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, E, Col[E @uV]]): Col[E @uV] = underlying.to_![Col]



  override def toString = s"View($underlying -> $cbf)"
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
  /** This implicit makes staged & stagedView available on all traversables. */
  implicit def withExtensions[Repr](coll: Repr)(implicit traversable: IsTraversableLike[Repr]) =
    new ExtensionMethods[traversable.A, Repr](
      StagedCollectionOps[traversable.A](traversable.conversion(coll)))
}
