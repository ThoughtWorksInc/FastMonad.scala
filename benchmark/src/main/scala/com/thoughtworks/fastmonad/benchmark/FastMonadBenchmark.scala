package com.thoughtworks.fastmonad.benchmark

import com.thoughtworks.fastmonad.Continuations.UnitContinuation
//import com.thoughtworks.fastmonad.TryT
import org.openjdk.jmh.annotations._

import scala.collection.mutable.ListBuffer
import scalaz.concurrent.{Task => ScalazTask}

/**
  * @author 杨博 (Yang Bo)
  */
@State(Scope.Benchmark)
class FastMonadBenchmark {

  @Benchmark
  def sequenceScalazS(): Long = {
    val tasks = (0 until 1000).map(_ => ScalazTask.delay(1)).toList
    val init = ScalazTask.delay(ListBuffer.empty[Int])
    tasks
      .foldLeft(init)((acc, elem) => acc.flatMap(lb => elem.map(e => lb += e)))
      .map(_.toList.sum.toLong)
      .unsafePerformSync
  }

  @Benchmark
  def sequenceFastMonadS(): Long = {
    val tasks =
      (0 until 1000).map(_ => (UnitContinuation.delay[Int](() => 1))).toList
    val init: UnitContinuation[ListBuffer[Int]] = (UnitContinuation.delay(() => ListBuffer.empty[Int]))
    tasks
      .foldLeft(init)((acc, elem) => acc.flatMap(lb => elem.map(e => lb += e)))
      .map(_.toList.sum.toLong)
      .blockingAwait

  }

}
