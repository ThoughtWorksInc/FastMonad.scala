package com.thoughtworks.fastmonad

import com.thoughtworks.fastmonad.Continuations.UnitContinuation

import scala.collection.mutable.ListBuffer

object Main {

  def main(args: Array[String]): Unit = {
    val tasks = (0 until 1000000).map(_ => UnitContinuation.delay[Int](() => 1)).toList
    val init: UnitContinuation[ListBuffer[Int]] = UnitContinuation.delay[ListBuffer[Int]](() => ListBuffer.empty[Int])
    tasks
      .foldLeft(init)((acc, elem) => acc.flatMap(lb => elem.map(e => lb += e)))
      .map(_.toList.sum.toLong)
      .blockingAwait

  }
}
