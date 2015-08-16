package com.jsuereth.collections

import org.scalameter._
import org.scalameter.api.Gen
import org.scalameter.api.PerformanceTest
import org.scalameter.api.Persistor
import org.scalameter.reporting.ChartReporter
import org.scalameter.reporting.ChartReporter.ChartFactory

import scala.collection.mutable.ArrayBuffer

/**
 * Created by jsuereth on 8/15/15.
 */
object SliceBenchmark extends PerformanceTest.Microbenchmark {
/*
  override def reporter = ChartReporter(ChartFactory.XYLine())
  def executor = new execution.LocalExecutor(
    Warmer.Default(),
    Aggregator.min,
    measurer
  )
  def persistor = Persistor.None
*/

  // multiple tests can be specified here
  val sizes: Gen[Int] = Gen.range("size")(300000, 1500000, 300000)
  val collection = sizes.map(r => (0 to r).to[ArrayBuffer])

  performance of "Slice" in {
    measure method "stagedView" in {
      using(collection  ) in {
        r =>
          import com.jsuereth.collections.View.withExtensions
          r.stagedView.slice(40000,40500).count(_ % 3 == 1)
      }
    }
    measure method "view" in {
      using(collection) in {
        r =>
          r.view.slice(40000,40500).count(_ % 3 == 1)
      }
    }

  }
}
