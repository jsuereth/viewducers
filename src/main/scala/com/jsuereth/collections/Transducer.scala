package com.jsuereth.collections

import Types.Fold

import scala.collection.GenTraversableOnce
import scala.collection.mutable.ArrayBuffer

/**
 * A transformer over Folds.   We use the correctly hype marketed name.
 *
 * Note:  A transducer is ALMOST a straight function, but we need to leave the Accumulator type unbound.  This
 *        simple trick allows us to compose these things.
 */
sealed abstract class Transducer[-A, +B] {
  // Note the order of composition is inverted from what you'd expect.
  def apply[Accumulator](in: Fold[Accumulator, B]): Fold[Accumulator, A]

  // TODO - Figure out how to implement this method with more meaning:
  final def optimise: Transducer[A, B] =
    this match {
      // TODO - we need to peel off the layers of JoinedTransducers and create Seq(ops) if we can, then optimise via the entire sequence.
      case JoinedTransducer(_: IdentityTransducer[_], other) => other.optimise.asInstanceOf[Transducer[A,B]]
      case JoinedTransducer(other, id) if id.isInstanceOf[IdentityTransducer[_]] => other.optimise.asInstanceOf[Transducer[A,B]]
      case JoinedTransducer(one, two) => JoinedTransducer(one.optimise, two.optimise)
      // TODO - is this actually more efficient, or are transducers just as fast?
      case JoinedTransducer(MapTransducer(f), MapTransducer(g)) => MapTransducer(f andThen g)
      case other => other
    }

  private def toSeqOps: Seq[Transducer[_,_]] = {
    def toSeqOpsImpl(acc: Seq[Transducer[_, _]], next: Transducer[_, _]): Seq[Transducer[_, _]] =
      next match {
        case JoinedTransducer(a @ JoinedTransducer(_,_), b) => toSeqOpsImpl(acc ++ toSeqOpsImpl(Vector.empty, a), b)
        case JoinedTransducer(a, b) => toSeqOpsImpl(acc :+ a, b)
        case other => acc :+ other
      }
    toSeqOpsImpl(Vector.empty, this)
  }

  // TODO - maybe we move this into an extension method mechanism instead...
  final def andThen[C](other: Transducer[B,C]): Transducer[A,C] = new JoinedTransducer(this, other)
}
object Transducer {
  def identity[A]: Transducer[A,A] = IdentityTransducer[A]()
  def map[A,B](f: A => B): Transducer[A,B] = MapTransducer(f)
  def filter[A](f: A => Boolean): Transducer[A,A] = FilterTransducer(f)
  def flatMap[A,B](f: A => GenTraversableOnce[B]) = FlatMapTransducer(f)
  def slice[A](start: Int, end: Int): Transducer[A,A] = SliceTransducer(start, end)
  def zipWithIndex[A]: Transducer[A, (A, Int)] = ZipWithIndexTransducer()
}

// A generic transducer for which we cannot perform any known optimisations/extensions
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