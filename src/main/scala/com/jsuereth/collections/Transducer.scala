package com.jsuereth.collections

import Types.Fold

import scala.collection.GenTraversableOnce
import scala.collection.mutable.ArrayBuffer

/**
 * A transformer over Folds.  Instead of calling it a "fold transformer" or "fold mapper" or anything so blaise, we'll
 * use a nicely marketed term which doesn't convey the meaning/implications immediately, but does well with SEO.
 *
 * Note:  A transducer is ALMOST a straight function, but we need to leave the Accumulator type unbound.
 *        In scala, these unbound universal types can be expressed via abstract classes + polymorphic inheritance.
 *        There is no convenient "lambda" syntax for these types, but we attempt to give the best we can for this concept.
 */
sealed abstract class Transducer[-A, +B] {
  /** Alter an existing fold function as specified by this transducer. */
  def apply[Accumulator](in: Fold[Accumulator, B]): Fold[Accumulator, A]

  /** Currently this method is in a beta-debugging mode.  The goal is for it to:
    *
    * - Flatten any Tree-like nesting structure so we have a linked-list structure
    * - Join together any operations which can be (map then flatMap e.g.)
    * - Remove any placeholder Transducers (identity)
    * - Reorder/Join limiting operations (e.g. multiple Slice operations)
    *
    * @return A new transducer with more optimal execution behavior
    */
  final def optimise: Transducer[A, B] = {
    // Note: We'e already done compile-time checks of types, so here we get dirty as we try to shuffle the bytecode around.
    def optimiseImpl(list: Seq[Transducer[_,_]], finalList: Seq[Transducer[_,_]]): Seq[Transducer[_,_]] = list match {
      case Seq() => finalList
      // TODO - we need to peel off the layers of JoinedTransducers and create Seq(ops) if we can, then optimise via the entire sequence.
      case Seq(_: IdentityTransducer[_], other, rest @ _*) => optimiseImpl(rest, finalList :+ other)
      case Seq(other, _: IdentityTransducer[_], rest @ _*) => optimiseImpl(rest, finalList :+ other)
      // TODO - Preliminary tests show combining map functions to be LESS efficient.
      // TODO - If the pattern matcher ever re-ifies the type and checksthem, we cry because it's broken.
      case Seq(head, rest @ _*) => optimiseImpl(rest, finalList :+ head)
    }
    // Reduce left appears to be faster for the JVM to execute than reduceRight, but could just be noise.
    optimiseImpl(toSeqOps, ArrayBuffer.empty).reduceLeft {
       (left, right) =>
      JoinedTransducer[Any,Any,Any](left.asInstanceOf[Transducer[Any,Any]],right.asInstanceOf[Transducer[Any,Any]])
    }.asInstanceOf[Transducer[A,B]]
  }

  /** An internal mechanism to convert a transducer into a Sequence of transducer operations.
    *
    * This is step one when optimising a transducer.
    */
  protected final def toSeqOps: Seq[Transducer[_,_]] = {
    def toSeqOpsImpl(acc: Seq[Transducer[_, _]], next: Transducer[_, _]): Seq[Transducer[_, _]] =
      next match {
        case JoinedTransducer(a @ JoinedTransducer(_,_), b) => toSeqOpsImpl(acc ++ toSeqOpsImpl(Vector.empty, a), b)
        case JoinedTransducer(a, b) => toSeqOpsImpl(acc :+ a, b)
        case other => acc :+ other
      }
    toSeqOpsImpl(Vector.empty, this)
  }

  /** Run another transducer after this transducer. */
  final def andThen[C](other: Transducer[B,C]): Transducer[A,C] = new JoinedTransducer(this, other)
}
/** Helper methods to create simple transducers. */
object Transducer {
  /** A transducer which returns the original fold. */
  def identity[A]: Transducer[A,A] = IdentityTransducer[A]()
  /* A transducer which maps elements in the fold to new elements. */
  def map[A,B](f: A => B): Transducer[A,B] = MapTransducer(f)
  /* A transducers which removes elements from the fold. */
  def filter[A](f: A => Boolean): Transducer[A,A] = FilterTransducer(f)
  /* A transducer which expands each element into multiple elements. */
  def flatMap[A,B](f: A => GenTraversableOnce[B]) = FlatMapTransducer(f)
  /** A transducer which slices the fold and only returns elements that fit inside a range. */
  def slice[A](start: Int, end: Int): Transducer[A,A] = SliceTransducer(start, end)
  /** A transducer which attaches an index to each element in the fold (note: the fold function returned is mutable). */
  def zipWithIndex[A]: Transducer[A, (A, Int)] = ZipWithIndexTransducer()
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
  override def apply[Accumulator](in: Fold[Accumulator, B]): Fold[Accumulator, A] = {
        // TODO - concrete class rather than anonymous function
    (acc, el) => in(acc, f(el))
  }
  override def toString = s"Map($f)"
}
private[collections] final case class FlatMapTransducer[A,B](f: A => GenTraversableOnce[B]) extends Transducer[A, B] {
  override def apply[Accumulator](in: Fold[Accumulator, B]): Fold[Accumulator, A] = {
    (acc, el) => f(el).foldLeft(acc)(in)
  }
  override def toString = s"FlatMap($f)"
}

private[collections] final case class FilterTransducer[A](f: A => Boolean) extends Transducer[A,A] {
  // TODO - concrete class over anonymous function.
  override def apply[Accumulator](in: Fold[Accumulator, A]): Fold[Accumulator, A] =
    (acc, el) =>
      if(f(el)) in(acc, el)
      else acc
  override def toString = s"Filter($f)"
}

private[collections] final case class IdentityTransducer[A]() extends Transducer[A,A] {
  override def apply[Accumulator](in: Fold[Accumulator, A]): Fold[Accumulator, A] = in
  override def toString = s"identity"
}

private[collections] final case class SliceTransducer[A](start: Int, end: Int) extends Transducer[A,A] {
  override def apply[Accumulator](in: Fold[Accumulator,A]): Fold[Accumulator,A] =
    new IndexStoringFoldFunction[Accumulator](in)
  //Mutable optimisation
  private class IndexStoringFoldFunction[Accumulator](next: Fold[Accumulator, A]) extends Fold[Accumulator, A] {
    var idx = -1
    override def apply(acc: Accumulator, el: A): Accumulator = {
      idx += 1
      if(idx >= start && idx < end)  next(acc, el)
      else acc
    }
  }

  override def toString = s"Slice($start, $end)"
}

private[collections] final case class ZipWithIndexTransducer[A]() extends Transducer[A, (A, Int)] {
  override def apply[Accumulator](in: Fold[Accumulator, (A, Int)]): Fold[Accumulator,A] =
    new IndexStoringFoldFunction[Accumulator](in)
  //Mutable optimisation
  private class IndexStoringFoldFunction[Accumulator](next: Fold[Accumulator, (A, Int)]) extends Fold[Accumulator, A] {
    var idx = -1
    override def apply(acc: Accumulator, el: A): Accumulator = {
      idx += 1
      next(acc, el -> idx)
    }
  }
  override def toString = s"ZipWithIndex"
}