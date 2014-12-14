A project based on: https://gist.github.com/odersky/6b7c0eb4731058803dfd.


VIEWDUCTION -  The intersection of Transducers (Fold transformers) and Scala collection Views.


This project attempts to improve the implementation & performance of Scala collection views via the following ideas:

* We do not need to memoize views, instead we need to make the user stage all desired operations and then execute as needed.
* We should do as little work as possible on most operations
* We should be able to do minor performane tweaks to staged operations
* We will make any operation which executes a view have an ugly character (`!`) so people know it's effectful
* Transducers represent an excellent way to stage computation that will be performed via a fold.
* We need extensive microbenchmarking to test out different possible optimisations and code traversals, as well as understand the overhead.
* We should be able to avoid excessive bytecode overhead through simple understanding of Scala => JVM features.

## Transducers

This library provides a very minimal and complete Transducers library.  This transducers library is optimised for use
with the Scala collections library in its current form.  We won't dig into the details of transducers in this readme,
but will set up a tutorial on them separately.


## StagedCollections

A Staged collection is a collection where you stage Transducers to execute against it.   Any operation devoid of a `!`
is one that simply append a new Transducer onto the stack.   The `toString` method of a StagedCollection will attempt to
show you the stack of transducers.  

**Note: The `toString` could grow out of control and is meant purely for debugging, not logging.**

If you're only interested in the new, delineated, API try the following.


```
> import com.jsuereth.collections._
import com.jsuereth.collections._

scala> val test = StagedCollectionOps(Vector(1,2,3))
test: com.jsuereth.collections.StagedCollectionOps[Int] = Vector(1, 2, 3) -> identity -> done

scala> val test2 = test.map(_ + 1).take(2).zipWithIndex
test2: com.jsuereth.collections.StagedCollectionOps[(Int, Int)] = Vector(1, 2, 3) -> identity -> Map(<function1>) -> Slice(0, 2) -> ZipWithIndex -> done

scala> test2.to_![Vector]
res0: Vector[(Int, Int)] = Vector((2,0), (3,1))
```

## Views

This library attempts to mimic, as much as it can, the existing view API via an abstraction on top of `StagedCollection`s, called `View`.
A View is something which will stage all the operations it can, but certain operations will force it to run against the original collection.

Note:  Unlike Scala's current views, these will NEVER memoize the partial-results of operations.   Upon terminal operation it will
attempt to read from the original collection again.  Like Scala's current views, these also do not denote which operations are terminal.  They
are meant as a drop-in replacement for existing views.

```
scala> import com.jsuereth.collections.View._
import com.jsuereth.collections.View._

scala> Vector(1,2,3,4).stagedView.filter(_%2==0)
res0: com.jsuereth.collections.View[Int,scala.collection.immutable.Vector[Int]] = SimpleView(Vector(1, 2, 3, 4) -> identity -> Filter(<function1>) -> done,scala.collection.IndexedSeq$$anon$1@5bf6e33b)

scala> res0.force
res1: scala.collection.immutable.Vector[Int] = Vector(2, 4)

scala> "Hello".stagedView.filter(_ % 2 == 0).force
res2: String = Hll

scala> "Hello".stagedView.filter(_ % 2 == 0)
res3: com.jsuereth.collections.View[scala.collection.generic.IsTraversableLike.stringRepr.A,String] = SimpleView(Hello -> identity -> Filter(<function1>) -> done,scala.Predef$$anon$3@450f9792)
```

