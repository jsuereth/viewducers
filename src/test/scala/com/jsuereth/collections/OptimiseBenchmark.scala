package com.jsuereth.collections

import org.scalameter.api._

import scala.collection.mutable.ArrayBuffer


object OptimiseBenchmark extends PerformanceTest.Quickbenchmark {
  // multiple tests can be specified here
  val sizes: Gen[Int] = Gen.range("size")(300000, 1500000, 300000)
  val collection: Gen[Vector[Int]] = sizes.map(r => (0 to r).to[Vector])
  val staged: Gen[StagedCollectionOps[Int]] = collection.map(StagedCollectionOps(_))

  performance of "early-exit-hook" in {
    measure method "Unoptimised" in {
      using(staged) in {
        r =>
          val ops = r.take(2).map(_+2).filter(_%2==0)
          val builder = ArrayBuffer.newBuilder[Int]
          ops.foldLeft_!(builder) {
            (acc, el) => acc += el
          }
          builder.result()
      }
    }
    measure method "Optimised" in {
      using(staged) in {
        r =>
          val ops = r.take(2).map(_+2).filter(_%2==0)
          val builder = ArrayBuffer.newBuilder[Int]
          Transducer.withEarlyExit {
            ops.foldLeft_!(builder) {
              (acc, el) => acc += el
            }
          }
          builder.result()
      }
    }
  }
}
