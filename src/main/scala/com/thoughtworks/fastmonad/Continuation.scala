package com.thoughtworks.fastmonad
import scala.annotation.tailrec

object Continuation {

  abstract class IO[R <: State[R], +A] extends Continuation[R, A] {
    def result(): A
    final def step(continue: A => Continuation[R, R]): Continuation[R, R] = {
      continue(result())
    }
  }

  trait State[Self <: State[Self]] {
    def next(): Continuation[Self, Self]
  }

  @tailrec
  final def reset[R <: State[R]](continue: Continuation[R, R]): R = {
    continue match {
      case io: IO[R, R] =>
        io.result()
      case tailCall =>
        reset(tailCall.step(_.next()))
    }
  }

}

trait Continuation[R <: Continuation.State[R], +A] {
  final def run(continue: A => Continuation[R, R]): R = {
    Continuation.reset(step(continue))
  }

  def step(continue: A => Continuation[R, R]): Continuation[R, R]

  def flatMap[B](f: A => Continuation[R, B]): Continuation[R, B] = { continue =>
    step { a =>
      f(a).flatMap(continue)
    }
  }
}
