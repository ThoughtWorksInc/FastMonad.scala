package com.thoughtworks.fastmonad
import scala.annotation.tailrec

object Continuation {

  abstract class IO[R <: Continuation[R, R], +A] extends Continuation[R, A] {
    def result(): A
    final def step(continue: A => Continuation[R, R]): Continuation[R, R] = {
      continue(result())
    }
  }

  @tailrec
  final def reset[R <: Continuation[R, R]](continue: Continuation[R, R]): R = {
    continue match {
      case io: IO[R, R] =>
        io.result()
      case tailCall =>
        reset(tailCall.step(identity[R]))
    }
  }

}

trait Continuation[R <: Continuation[R, R], +A] extends Any with Monad[A] {

  type F[+A] = Continuation[R, A]
  type G[+A] = Continuation[R, A]

  final def run(continue: A => Continuation[R, R]): R = {
    Continuation.reset(step(continue))
  }

  def map[B](f: (A) => B): Continuation[R, B] = { continueB =>
    step { a => continueR =>
      continueB(f(a))
    }
  }

  def step(continue: A => Continuation[R, R]): Continuation[R, R]

  def flatMap[B](f: A => Continuation[R, B]): Continuation[R, B] = { continue =>
    step { a =>
      f(a).flatMap(continue)
    }
  }
}
