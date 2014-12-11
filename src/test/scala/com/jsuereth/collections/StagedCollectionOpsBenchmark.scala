package com.jsuereth.collections

import org.scalameter.api._

object StagedCollectionOpsBenchmark extends PerformanceTest {
  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val persistor = Persistor.None
  lazy val reporter = Reporter.Composite(
    new RegressionReporter(
      RegressionReporter.Tester.OverlapIntervals(),
      RegressionReporter.Historian.ExponentialBackoff() ),
    HtmlReporter(true)
  )//ChartReporter(ChartFactory.XYLine())

  // multiple tests can be specified here
  val sizes: Gen[Int] = Gen.range("size")(300000, 1500000, 300000)
  val collection: Gen[Vector[Int]] = sizes.map(r => (0 to r).to[Vector])
  val staged: Gen[StagedCollectionOps[Int]] = collection.map(StagedCollectionOps(_))

  performance of "Range" in {
    measure method "map" in {
      using(collection) in {
        r => r.map(_ + 1)
      }
    }
    measure method "map-then-filter-then-zip" in {
      using(collection) in {
        r => r.map(_ + 1).filter(_ % 2 == 0).zipWithIndex
      }
    }
  }

  performance of "Staged" in {
    measure method "map" in {
      using(staged) in {
        r => r.map(_ + 1).to_![Seq]
      }
    }
    measure method "map-then-filter-then-zip" in {
      using(staged) in {
        r => r.map(_ + 1).filter(_ % 2 == 0).zipWithIndex.to_![Seq]
      }
    }
  }
}