package com.jsuereth.collections

import org.scalameter.api._
import View.withExtensions

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
  val stagedView = collection.map(_.stagedView)
  val scalaView = collection.map(_.view)

  performance of "map-then-filter-then-zip" in {
    measure method "Vector" in {
      using(collection) in {
        r => r.map(_ + 1).filter(_ % 2 == 0).zipWithIndex
      }
    }
    measure method "Staged" in {
      using(staged) in {
        r => r.map(_ + 1).filter(_ % 2 == 0).zipWithIndex.to_![Seq]
      }
    }

    measure method "StagedView" in {
      using(stagedView) in {
        r => r.map(_ + 1).filter(_ % 2 == 0).zipWithIndex.force
      }
    }

    measure method "Scala View" in {
      using(scalaView) in {
        r => r.map(_ + 1).filter(_ % 2 == 0).zipWithIndex.force
      }
    }

  }



}