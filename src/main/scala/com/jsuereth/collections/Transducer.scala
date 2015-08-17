package com.jsuereth.collections

import Types.Fold

import scala.collection.GenTraversableOnce
import scala.collection.mutable.ArrayBuffer
import scala.util.control.ControlThrowable

/**
 * A transformer over Folds.  Instead of calling it a "fold transformer" or "fold mapper" or anything so blaise, we'll
 * use a nicely marketed term which doesn't convey the meaning/implications immediately, but does well with SEO.
 *
 * Note:  A transducer is ALMOST a straight function, but we need to leave the Accumulator type unbound.
 *        In scala, these unbound universal types can be expressed via abstract classes + polymorphic inheritance.
 *        There is no convenient "lambda" syntax for these types, but we attempt to give the best we can for this concept.
 */
sealed abstract class Transducer[-A, +B] {
  /**
   * Alter an existing fold function as specified by this transducer.
   *
   * Note:  The resulting function may or may not be pure, depending on the Transducer used and whether the
   *        input function is pure.
   */
  def apply[Accumulator](in: Fold[Accumulator, B]): Fold[Accumulator, A]

  /** Run another transducer after this transducer. */
  final def andThen[C](other: Transducer[B,C]): Transducer[A,C] = new JoinedTransducer(this, other)
}
/**
 * Helper methods to create simple transducers.
 *
 */
object Transducer {
  /** A transducer which returns the original fold. */
  def identity[A]: Transducer[A,A] = IdentityTransducer[A]()
  /** A transducer which maps elements in the fold to new elements. */
  def map[A,B](f: A => B): Transducer[A,B] = MapTransducer(f)
  /**
   * A transducer which transforms and collects elements which mach the partial function.
   */
  def collect[A,B](f: PartialFunction[A,B]): Transducer[A,B] = CollectTransducer(f)
  /**
   * A transducers which removes elements from the fold.
   */
  def filter[A](f: A => Boolean): Transducer[A,A] = FilterTransducer(f)
  /**
   * A transducer which expands each element into multiple elements.
   */
  def flatMap[A,B](f: A => GenTraversableOnce[B]) = FlatMapTransducer(f)
  /**
   * A transducer which slices the fold and only returns elements that fit inside a range.
   *
   * Note: The fold function returned is mutable.
   */
  def slice[A](start: Int, end: Int): Transducer[A,A] = SliceTransducer(start, end)
  /**
   * A transducer which attaches an index to each element in the fold
   *
   * Note: the fold function returned is mutable.
   */
  def zipWithIndex[A]: Transducer[A, (A, Int)] = ZipWithIndexTransducer()

  /** takes elements in the reduction until the condition returns false.
    *
    * Note: The reduce function returned will be stateful/mutable, while this Transducer itself is immutable.
    *
    * @param f The predicate to test when we stop taking elements.
    *
    * @tparam A The type of elements we accept.
    * @return  A transducer which limits the input based on the predicate.
    */
  def takeWhile[A](f: A => Boolean) = TakeWhileTransducer(f)
  /** takes elements in the reduction until the condition returns false.
    *
    * Note: The reduce function returned will be stateful/mutable, while this Transducer itself is immutable.
    *
    * @param f The predicate to test when we stop taking elements.
    *
    * @tparam A The type of elements we accept.
    * @return  A transducer which limits the input based on the predicate.
    */
  def dropWhile[A](f: A => Boolean) = DropWhileTransducer(f)

  /** The exception we throw to terminate execution early. */
  private class EarlyExit[Accumulator](val accumulator: Accumulator) extends ControlThrowable {
    override def fillInStackTrace(): Throwable = this
  }
  // Hidden thread local which supports the earlyExit + withEarlyExit API.
  private object supportsEarlyExitLocal extends ThreadLocal[Boolean] {
    override def initialValue: Boolean = false
  }

  /**
   * A rather evil method to help optimise single-threaded transducer calls.   A transducer may call this
   * and if the stack he is on supports early exit handling, then it will throw an early exit signal.
   *
   * The design of this feature is somewhat suspect, but acts as a compromise of convenience, performance and
   * JVM friendlyness.
   *
   * @param acc  The accuulator to return
   * @tparam Accumulator  The type of the Accumulator
   * @return  The accumulator (with a "drop of the reduce function" hook if enabled).
   */
  private[collections] def earlyExit[Accumulator](acc: Accumulator): Accumulator = throw new EarlyExit(acc)

  /** Returns true if a transducer should return an early-exit reducer function. */
  private[collections] def shouldEarlyExit: Boolean = supportsEarlyExitLocal.get()

  /**
   * This denotes a block of code as handling early exits.
   *
   * Note:  The code being run *should* block the current thread until completed.  This is a consequence of attempting
   * to attain sufficient single-threaded behavior using a similar early exit strategy which was used previously for
   * views.
   *
   * Note 2:  If you don't want this sort of hackery, we should stick with Iteratees which accomplish the same thing as
   * Transducers w/ more flexibility and less efficiency.  We assume here efficiency is the god we sacrifice our clean code against.
   * (And yes, we ran performance tests).
   * i.e. Don't use this method unless you understand the architecture of this Library.
   * @param f  a function to run which should support early-exit semantics for efficiency.
   * @tparam Accumulator  The accumulator type we will return.
   * @return  The result of the function.
   */
  // Note: This will handle early exit exceptions from reducers that support them.
  private[collections] def withEarlyExit[Accumulator](f: => Accumulator) = {
    val orig = supportsEarlyExitLocal.get()
    supportsEarlyExitLocal.set(true)
    try f
    catch {
      case err: EarlyExit[Accumulator] => err.accumulator
    }
    finally supportsEarlyExitLocal.set(orig)
  }
}

/**
 * A generic transducer for users to extend.
 *
 * Note: We cannot perform any optimisations around generic transducers.
 */
abstract class GenericTransducer[A,B] extends Transducer[A,B]

// A Joined fold over two collections.
private[collections] final case class JoinedTransducer[A, B, C](left: Transducer[A,B], right: Transducer[B,C]) extends Transducer[A,C] {
  override def apply[Accumulator](in: Fold[Accumulator, C]): Fold[Accumulator, A] = left(right(in))
  override def toString = s"$left -> $right"
}


private[collections] final case class MapTransducer[A,B](f: A => B) extends Transducer[A,B] {
  override def apply[Accumulator](in: Fold[Accumulator, B]): Fold[Accumulator, A] = new MyFold(in)
  private class MyFold[Accumulator](in: Fold[Accumulator, B]) extends AbtractFold[Accumulator, A] {
    override def apply(acc: Accumulator, el: A): Accumulator =
      in(acc, f(el))
  }
  override def toString = s"Map($f)"
}

private[collections] final case class CollectTransducer[A,B](f: PartialFunction[A,B]) extends Transducer[A,B] {
  val lifted = f.lift
  override def apply[Accumulator](in: Fold[Accumulator, B]): Fold[Accumulator, A] = new MyFold(in)
  private class MyFold[Accumulator](in: Fold[Accumulator, B]) extends AbtractFold[Accumulator, A] {
    override def apply(acc: Accumulator, el: A): Accumulator =
      lifted(el) match {
        case Some(fel) => in(acc, fel)
        case None => acc
      }
  }
  override def toString = s"Collect($f)"
}
private[collections] final case class FlatMapTransducer[A,B](f: A => GenTraversableOnce[B]) extends Transducer[A, B] {
  override def apply[Accumulator](in: Fold[Accumulator, B]): Fold[Accumulator, A] = new MyFold(in)
  private class MyFold[Accumulator](in: Fold[Accumulator, B]) extends AbtractFold[Accumulator, A] {
    override def apply(acc: Accumulator, el: A): Accumulator = f(el).foldLeft(acc)(in)
  }
  override def toString = s"FlatMap($f)"
}

private[collections] final case class FilterTransducer[A](f: A => Boolean) extends Transducer[A,A] {
  // TODO - concrete class over anonymous function.
  override def apply[Accumulator](in: Fold[Accumulator, A]): Fold[Accumulator, A] = new FilterFold(in)
  // We make a local class for the debugging.
  private class FilterFold[Accumulator](next: Fold[Accumulator, A]) extends AbtractFold[Accumulator, A] {
    override def apply(acc: Accumulator, el: A): Accumulator =
      if(f(el)) next(acc, el)
      else acc
  }
  override def toString = s"Filter($f)"
}

private[collections] final case class IdentityTransducer[A]() extends Transducer[A,A] {
  override def apply[Accumulator](in: Fold[Accumulator, A]): Fold[Accumulator, A] = in
  override def toString = s"identity"
}

private[collections] final case class SliceTransducer[A](start: Int, end: Int) extends Transducer[A,A] {

  // NOTE: Returning two functions like this can shave ~10% off the execution speed of the non-early exit path
  //       and largely doesn't affect the early-exit path.
  override def apply[Accumulator](in: Fold[Accumulator,A]): Fold[Accumulator,A] =
    if(Transducer.shouldEarlyExit) new StatefulEarlyFoldFunction[Accumulator](in)
    else new StatefulFoldFunction[Accumulator](in)
  //Mutable optimisation
  private class StatefulFoldFunction[Accumulator](next: Fold[Accumulator, A]) extends AbtractFold[Accumulator, A] {
    var idx = -1
    override def apply(acc: Accumulator, el: A): Accumulator = {
      idx += 1
      if(idx >= start && idx < end)  next(acc, el)
      else acc
    }
  }
  // Super crazy optimisatoin.
  private class StatefulEarlyFoldFunction[Accumulator](next: Fold[Accumulator, A]) extends AbtractFold[Accumulator, A] {
    var idx = -1
    override def apply(acc: Accumulator, el: A): Accumulator = {
      idx += 1
      if(idx < end)  {
        if(idx >= start) next(acc, el)
        else acc
      }
      else {
        Transducer.earlyExit(acc)
      }
    }
  }
  override def toString = s"Slice($start, $end)"
}

private[collections] final case class TakeWhileTransducer[A](f: A => Boolean) extends Transducer[A,A] {
  override def apply[Accumulator](in: Fold[Accumulator,A]): Fold[Accumulator,A] =
    new StatefulFoldFunction[Accumulator](in)
  //Mutable optimisation
  private class StatefulFoldFunction[Accumulator](next: Fold[Accumulator, A]) extends AbtractFold[Accumulator, A] {
    var taking = true
    override def apply(acc: Accumulator, el: A): Accumulator = {
      taking = taking && f(el)
      if(taking)  next(acc, el)
      else acc
    }
  }
  override def toString = s"TakeWhile($f)"
}

private[collections] final case class DropWhileTransducer[A](f: A => Boolean) extends Transducer[A,A] {
  override def apply[Accumulator](in: Fold[Accumulator,A]): Fold[Accumulator,A] =
    new StatefulFoldFunction[Accumulator](in)
  //Mutable optimisation
  private class StatefulFoldFunction[Accumulator](next: Fold[Accumulator, A]) extends AbtractFold[Accumulator, A] {
    var dropping = true
    override def apply(acc: Accumulator, el: A): Accumulator = {
      dropping = dropping && f(el)
      if(dropping)  acc
      else next(acc, el)
    }
  }
  override def toString = s"DropWhile($f)"
}

private[collections] final case class ZipWithIndexTransducer[A]() extends Transducer[A, (A, Int)] {
  override def apply[Accumulator](in: Fold[Accumulator, (A, Int)]): Fold[Accumulator,A] =
    new StatefulFoldFunction[Accumulator](in)
  //Mutable optimisation
  private class StatefulFoldFunction[Accumulator](next: Fold[Accumulator, (A, Int)]) extends AbtractFold[Accumulator, A] {
    var idx = -1
    override def apply(acc: Accumulator, el: A): Accumulator = {
      idx += 1
      next(acc, el -> idx)
    }
  }
  override def toString = s"ZipWithIndex"
}