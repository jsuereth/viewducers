package com.jsuereth.collections

import org.scalatest._
import View.withExtensions

class ViewSpec extends FunSuite with Matchers {
  test("count") {
    val orig = List(1,2,3,4)
    val scalaView = orig.view.count(_ % 2 == 0)
    val ourView = orig.stagedView.count(_ % 2 == 0)
    ourView should equal(scalaView)
  }

  test("appendMultiple") {
    val scalaView = (Vector(1,2,3).view ++ Vector(4)).force
    // TODO - ours should force too...
    val ourView = (Vector(1,2,3).stagedView ++ Vector(4))
    ourView should equal(scalaView)
  }

  test("flatten") {
    val orig = (Vector(Vector(1,2), Vector(3,4)))
    val scalaView = orig.view.flatten.force
    val ourView = orig.stagedView.flatten.force
    ourView should equal(scalaView)
  }

  test("drop") {
    val orig = (1 to 20).to[scala.collection.mutable.ArrayBuffer]
    val scalaView = orig.view.drop(10).force
    val ourView = orig.stagedView.drop(10).force
    ourView should equal(scalaView)
  }

  test("find") {
    val orig = (1 to 20).to[scala.collection.mutable.Set]
    val scalaView = orig.view.find(_ % 2 == 0)
    val ourView = orig.stagedView.find(_ % 2 == 0)
    ourView should equal(scalaView)
  }

  test("init") {
    val orig = (1 to 20).to[scala.collection.mutable.ArrayBuffer]
    val scalaView = orig.view.init.force
    val ourView = orig.stagedView.init.force
    ourView should equal(scalaView)
  }

  test("forall") {
    val orig = (1 to 20).to[scala.collection.immutable.List]
    val scalaView = orig.view.forall(_%2 !=3)
    val ourView = orig.stagedView.forall(_%2 !=3)
    ourView should equal(scalaView)
  }

  test("exists") {
    val orig = (1 to 20).to[Vector]
    val scalaView = orig.view.exists(_ == 10)
    val ourView = orig.stagedView.exists(_==10)
    ourView should equal(scalaView)
  }
}
