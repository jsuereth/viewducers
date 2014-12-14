package com.jsuereth.collections

import org.specs2._
import View.withExtensions

object ViewSpec extends Specification {
  override def is = s2"""
       This is the specification for the View class.


       The View class should
          append elements with ++ $appendMultiple
          count elements          $count
          flatten nested collections $flatten
          drop elements           $drop
  """

  def count = {
    val orig = List(1,2,3,4)
    val scalaView = orig.view.count(_ % 2 == 0)
    val ourView = orig.stagedView.count(_%2==0)
    ourView must beEqualTo(scalaView)
  }

  def appendMultiple = {
    val scalaView = (Vector(1,2,3).view ++ Vector(4)).force
    // TODO - ours should force too...
    val ourView = (Vector(1,2,3).stagedView ++ Vector(4))
    ourView must beEqualTo(scalaView)
  }

  def flatten = {
    val orig = (Vector(Vector(1,2), Vector(3,4)))
    val scalaView = orig.view.flatten.force
    val ourView = orig.stagedView.flatten.force
    ourView must beEqualTo(scalaView)
  }

  def drop = {
    val orig = (1 to 20).to[scala.collection.mutable.ArrayBuffer]
    val scalaView = orig.view.drop(10).force
    val ourView = orig.stagedView.drop(10).force
    ourView must beEqualTo(scalaView)
  }
}
