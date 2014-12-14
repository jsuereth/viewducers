package com.jsuereth.collections

import org.scalameter.api._
import View._
import scala.collection.mutable.ArrayBuffer


object OptimiseBenchmark extends PerformanceTest.Quickbenchmark {
  // multiple tests can be specified here
  val sizes: Gen[Int] = Gen.range("size")(300000, 1500000, 300000)
  val collection: Gen[Vector[Int]] = sizes.map(r => (0 to r).to[Vector])
  val staged: Gen[View[Int, Vector[Int]]] = collection.map(_.stagedView)

  performance of "Memoize" in {
    measure method "Optimised" in {
      using(staged) in {
        r =>
          val firstOps = r.map(_ + 1).flatMap(i => 1 to 10).memoize
          val size = firstOps.size
          val result = firstOps.to[Array]
          result(1) + size

      }
    }
    measure method "Unoptimised" in {
      using(staged) in {
        r =>
          val firstOps = r.map(_ + 1).flatMap(i => 1 to 10)
          val size = firstOps.size
          val result = firstOps.to[Array]
          result(1) + size
      }
    }
  }
}
