package com.thoughtworks.fastmonad

import com.thoughtworks.fastmonad.Continuation.AnyContinuation

import scala.collection.mutable.ListBuffer


object Main {

  def main(args: Array[String]): Unit = {
    val tasks = (0 until 1000).map(_ => AnyContinuation.delay(() => 1)).toList
    val init: AnyContinuation[ListBuffer[Int]] = AnyContinuation.delay(() => ListBuffer.empty[Int])
    AnyContinuation.blockingAwait(
      tasks
        .foldLeft(init)((acc, elem) => acc.flatMap(lb => elem.map(e => lb += e)))
        .map(_.toList.sum.toLong)
    )

  }
}