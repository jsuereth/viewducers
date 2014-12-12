package com.jsuereth.collections

import org.scalameter.api._

import scala.collection.mutable.ArrayBuffer


object OptimiseBenchmark extends PerformanceTest.Quickbenchmark {
  // multiple tests can be specified here
  val sizes: Gen[Int] = Gen.range("size")(300000, 1500000, 300000)
  val collection: Gen[Vector[Int]] = sizes.map(r => (0 to r).to[Vector])
  val staged: Gen[StagedCollectionOps[Int]] = collection.map(StagedCollectionOps(_))
  val optimised: Gen[StagedCollectionOps[Int]] = staged.map(_.optimise)

  performance of "map-then-map-then-map-then-filter" in {
    measure method "Unoptimised" in {
      using(staged) in {
        r => r.map(_ + 1).map(_+2).filter(_ % 2 == 0).to_![ArrayBuffer]
      }
    }
    measure method "Optmised" in {
      using(optimised) in {
        r => r.map(_ + 1).map(_+2).filter(_ % 2 == 0).to_![ArrayBuffer]
      }
    }
  }
}
