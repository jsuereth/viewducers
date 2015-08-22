package com.jsuereth.collections

import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.language.higherKinds
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

/** A collection of staged operations (transducers) that we build up against a source collection type.
  *
  * E.g., this could be a set of Map/FlatMap/Filter/slice operations to perform on a collection.   We assume
  * all collection methods can be aggregated into transducers.
  */
abstract class StagedCollectionOps[E] {

  /** The type of the elements of the source sequence */
  type SourceElement

  /** The source collection.  TODO - can we leave this unbound? */
  val source: GenTraversableOnce[SourceElement]

  /** The fold transformer, as a collection of operations. */
  def ops: Transducer[SourceElement, E]

  /** Returns true if the underlying collection can be traversed multiple times. */
  final def isTraversableAgain = source.isTraversableAgain

  /** Returns whether or not the original collection has a definite size.
    *
    * Note: we cannot really determine if the staged collection has a definite size, as
    *       any flatMap operations COULD be returning infinite collections.
    */
  final def hasDefiniteSourceSize = source.hasDefiniteSize
  /** Returns true if this collection has any staged operations. */
  final def hasStagedOperations = !ops.isInstanceOf[IdentityTransducer[_]]


  // Staging operations, TODO - document them.

  /** Create new view by composing current fold transformer with a new one */
  final def andThen[C](t: Transducer[E, C]): StagedCollectionOps[C] = {
    // TODO - should we always optimise the ops tree here?
    if(ops.isInstanceOf[IdentityTransducer[_]]) new SimpleStagedCollectionOps(source, t.asInstanceOf[Transducer[SourceElement, C]])
    else new SimpleStagedCollectionOps(source, (ops andThen t))
  }
  final def map[B](f: E => B): StagedCollectionOps[B] = andThen(Transducer.map(f))
  final def collect[B](f: PartialFunction[E,B]): StagedCollectionOps[B] = andThen(Transducer.collect(f))
  final def flatMap[B](f: E => GenTraversableOnce[B]) = andThen(Transducer.flatMap(f))
  final def filter(f: E => Boolean): StagedCollectionOps[E] = andThen(Transducer.filter(f))
  final def filterNot(f: E => Boolean): StagedCollectionOps[E] = andThen(Transducer.filter(e => !f(e)))  // TODO - Is this much slower?
  final def slice(start: Int, end: Int): StagedCollectionOps[E] = andThen(Transducer.slice(start,end))
  final def take(n: Int): StagedCollectionOps[E] = andThen(Transducer.slice(0, n))
  final def drop(n: Int): StagedCollectionOps[E] = andThen(Transducer.slice(n, Int.MaxValue))
  final def tail: StagedCollectionOps[E] = andThen(Transducer.slice(1, Int.MaxValue))
  final def zipWithIndex: StagedCollectionOps[(E, Int)] = andThen(Transducer.zipWithIndex[E])
  final def takeWhile(f: E => Boolean) = andThen(Transducer.takeWhile(f))
  final def dropWhile(f: E => Boolean) = andThen(Transducer.dropWhile(f))
  final def init = andThen(Transducer.init)
  // TODO - zip.... may not be possible to do correctly.....



  // Terminal operations



  /** Note - this will consume the traversable. */
  final def foldLeft_![Accumulator](acc: Accumulator)(f: (Accumulator, E) => Accumulator): Accumulator =
    // Note: We have tried to re-order operations here, and the cost of doing so is far outweighed by the raw computation.
    Transducer.withEarlyExit {
      source.foldLeft(acc)(ops.apply(f))
    }

  final def find_!(f: E => Boolean): Option[E] =
    // TODO - Check if early exit is supported...
    foldLeft_!(Option.empty[E]) {
      case (None, element) => if (f(element)) Transducer.earlyExit(Some(element)) else None
      case (result, _) => result
    }
  final def size_! : Int =
    if(isTraversableAgain && !hasStagedOperations) source.size
    else foldLeft_!(0)(Types.countingFold)

  final def isEmpty_! : Boolean =
    if(isTraversableAgain && !hasStagedOperations) source.isEmpty
    else size_! != 0

  final def to_![Col[_]](implicit cbf: CanBuildFrom[Nothing, E, Col[E @uV]]): Col[E @uV] = {
    val builder = cbf()
    // TODO - Ideally we'd like a *fast* way to know if we can send a size hint to the builder.
    if(isTraversableAgain && !hasStagedOperations) builder.sizeHint(source.size)
    foldLeft_!(builder)(Types.appendFold)
    builder.result()
  }
  override def toString = s"$source -> $ops -> done"
}
object StagedCollectionOps {
  /** Create a new set of staged operations against an original collection. */
  def apply[E](collection: GenTraversableOnce[E]): StagedCollectionOps[E] =
    new SimpleStagedCollectionOps[E, E](collection, IdentityTransducer[E]())
}
/** A simple implementation of StagedCollectionOps with no frills.
  *
  * We use this class to *hide* the original source element type from the type signatures.
  */
private[collections] final class SimpleStagedCollectionOps[Origin, Next](
  override val source: GenTraversableOnce[Origin],
  override val ops: Transducer[Origin, Next]
) extends StagedCollectionOps[Next] {
  type SourceElement = Origin
}