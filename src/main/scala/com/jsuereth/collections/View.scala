package com.jsuereth.collections

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.immutable.IndexedSeq
import scala.collection._
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions


/** A view represents a set of staged collection operations against an original collection.
  * Additionally, a view attempts to keep track of the most optimal collection to return from the
  * series of operations, using the `CanBuildFrom` pattern.
  *
  * @tparam E  The elements in the collection.
  * @tparam To The resulting collection type of the staged operations acrued so far.
  */
abstract class View[E, To] extends GenTraversableOnce[E] {
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
    underlying.foldLeft_!(builder)(Types.appendFold)
    xs.foldLeft(builder)(Types.appendFold)
    // SO, in new fun news, we need to return a VIEW here, yeah...
    builder.result()
  }
  final def count(p: E => Boolean): Int =
    underlying.foldLeft_!(0) { (acc, el) =>
      if (p(el)) acc + 1
      else acc
    }
  final def exists(p: E => Boolean): Boolean = find(p).isDefined
  final def find(p: E => Boolean): Option[E] = underlying.find_!(p)

  final def fold[A1 >: E](z: A1)(op: (A1, A1) ⇒ A1): A1 = underlying.foldLeft_!(z)(op)
  final def foldLeft[Accumulator](z: Accumulator)(op: (Accumulator, E) ⇒ Accumulator): Accumulator = underlying.foldLeft_!(z)(op)
  final def reduceLeftOption[B >: E](op: (B, E) => B): Option[B] = {
    var first = true
    var acc: Option[B] = None
    foldLeft(null) { (ignore, x) =>
      if (!acc.isDefined) {
        acc = Some(x)
      }
      else {
        acc = Some(op(acc.get, x))
      }
      null
    }
    acc
  }
  final def reduceLeft[B >: E](op: (B,E) => B): B = {
    reduceLeftOption(op).getOrElse(throw new UnsupportedOperationException("empty.reduceLeft"))
  }
  final def reduce[A1 >: E](op: (A1, A1) => A1): A1 = reduceLeft(op)
  final def aggregate[B](z: =>B)(seqop: (B, E) => B, combop: (B, B) => B): B = foldLeft(z)(seqop)
  final def min[B >: E](implicit cmp: Ordering[B]): E =
    reduceLeftOption((x, y) => if (cmp.lteq(x, y)) x else y).getOrElse(throw new UnsupportedOperationException("empty.min"))
  final def max[B >: E](implicit cmp: Ordering[B]): E = {
    reduceLeftOption((x, y) => if (cmp.gteq(x, y)) x else y).getOrElse(throw new UnsupportedOperationException("empty.max"))
  }
  final def maxBy[B](f: (E) => B)(implicit cmp: Ordering[B]): E =
    foldLeft[Option[(E, B)]](None) { (prev, el) =>
      prev match {
        case None => Some(el -> f(el))
        case Some((el1, value)) =>
          val newVal = f(el)
          if(cmp.gteq(value, newVal)) Some(el1 -> value)
          else Some(el -> newVal)
      }
    }.map(_._1).getOrElse(throw new UnsupportedOperationException("empty.maxBy"))
  final def minBy[B](f: (E) => B)(implicit cmp: Ordering[B]): E = {
    foldLeft[Option[(E, B)]](None) { (prev, el) =>
      prev match {
        case None => Some(el -> f(el))
        case Some((el1, value)) =>
          val newVal = f(el)
          if(cmp.lteq(value, newVal)) Some(el1 -> value)
          else Some(el -> newVal)
      }
    }.map(_._1).getOrElse(throw new UnsupportedOperationException("empty.minBy"))
  }
  final def foreach[U](f: (E) => U): Unit = foldLeft(()) { (ignore, el) => f(el) }
  final def /:[B](z: B)(op: (B, E) => B): B = foldLeft(z)(op)
  final def hasDefiniteSize: Boolean =
    underlying.hasDefiniteSourceSize && !underlying.hasStagedOperations


  final def reduceOption[A1 >: E](op: (A1, A1) => A1): Option[A1] = reduceLeftOption(op)
  final def reduceRightOption[B >: E](op: (E, B) => B): Option[B] =
    foldRight[Option[B]](None) { case (el, acc) =>
      acc match {
        case None => Some(el)
        case Some(el2) => Some(op(el, el2))
      }
    }
  final def foldRight[B](z: B)(op: (E, B) => B): B =
    // TODO - maybe a faster version?  We *may* be able to have the transducers support foldRight semantics...
    toStream.foldRight(z)(op)
  final def reduceRight[B >: E](op: (E, B) => B): B =
      reduceRightOption(op).getOrElse(throw new UnsupportedOperationException("empty.reduceRight"))
  final def :\[B](z: B)(op: (E, B) => B): B = foldRight(z)(op)

  final def nonEmpty: Boolean =
    if(hasDefiniteSize) !isEmpty
    else size > 0


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
  final def init: View[E,To] = SimpleView(underlying.init, cbf)
  // TDOO - inits
  final def isTraversableAgain: Boolean = underlying.isTraversableAgain
  final def isEmpty: Boolean = underlying.isEmpty_!
  final def size: Int = underlying.size_!
  final def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, E, Col[E @uV]]): Col[E @uV] = underlying.to_![Col]
  final def toSet[A1 >: E]: GenSet[A1] = to[immutable.Set].asInstanceOf[GenSet[A1]]
  final def toSeq: GenSeq[E] = toList

  final def toBuffer[A1 >: E]: mutable.Buffer[A1] = to[ArrayBuffer].asInstanceOf[mutable.Buffer[A1]]
  final def toStream: Stream[E] = toBuffer.toStream
  final def toArray[A1 >: E](implicit evidence$1: ClassManifest[A1]): Array[A1] = toBuffer.toArray
  final def toIterator: Iterator[E] = toBuffer.iterator
  final def toVector: Vector[E] = to[Vector]
  final def toList: List[E] = to[List]
  final def toMap[K, V](implicit ev: <:<[E, (K, V)]): GenMap[K, V] = {
    val b = immutable.Map.newBuilder[K, V]
    for (x <- this) b += x
    b.result()
  }
  final  def seq: scala.TraversableOnce[E] = toBuffer
  final def toIndexedSeq: IndexedSeq[E] = toVector
  final def toIterable: GenIterable[E] = toBuffer
  final def toTraversable: GenTraversable[E] =
    // TODO - Return this
    toBuffer
  final def copyToArray[B >: E](xs: Array[B]): Unit =  copyToArray(xs, 0, xs.length)
  final def copyToArray[B >: E](xs: Array[B], start: Int): Unit = copyToArray(xs, start, xs.length - start)
  final def copyToArray[B >: E](xs: Array[B], start: Int, len: Int): Unit = {
    // TODO - We could probably use "take" and foreach/zipWithIndex if we wanted.
    var i = start
    val end = (start + len) min xs.length
    import util.control.Breaks._
    breakable {
      for (x <- this) {
        if (i >= end) break
        xs(i) = x
        i += 1
      }
    }
  }



  // SUmmation/addition things.
  final def sum[B >: E](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)
  final def product[B >: E](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)


  final def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString
  final def mkString(sep: String): String = mkString("", sep, "")
  final def mkString: String = mkString("")
  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    var first = true
    b append start
    foldLeft(true) { (first, x) =>
      if(first) {
        b append x
      } else {
        b append sep
        b append x
      }
      false
    }
    b append end
    b
  }
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
