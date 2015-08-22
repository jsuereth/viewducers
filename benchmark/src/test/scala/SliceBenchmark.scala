package com.jsuereth.collections

import View._
import org.scalameter.api._
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
           r.stagedView.slice(40000,40500).count(_ % 3 == 1)
       }
     }
     /*measure method "view" in {
       using(collection) in {
         r =>
           r.view.slice(40000,40500).count(_ % 3 == 1)
       }
     }*/

   }
 }
