A toy project based on: https://gist.github.com/odersky/6b7c0eb4731058803dfd.


VIEWDUCTION -  The intersection of Transducers (Fold transformers) and Scala collection Views.


This project attempts to improve the implementation & performance of Scala collection views via the following ideas:

* We do not need to memoize views, instead we need to make the user stage all desired operations and then execute as needed.
* We should do as little work as possible on most operations
* We should be able to do minor performane tweaks to staged operations
* We will make any operation which executes a view have an ugly character (`!`) so people know it's effectful
* Transducers represent an excellent way to stage computation that will be performed via a fold.
* We need extensive microbenchmarking to test out different possible optimisations and code traversals, as well as understand the overhead.


## Basic Usage


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


## Apeing Existing collection views

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

