package com.thoughtworks.fastmonad
import scala.annotation.tailrec

object Continuation {

  class State extends Continuation[State] {
    def step(continue: (State) => Continuation[State]): Continuation[State] = {
      continue(this)
    }
  }

  abstract class IO[+A] extends Continuation[A] {
    def result(): A
    final def step(continue: A => Continuation[State]): Continuation[State] = {
      continue(result())
    }
  }

  @tailrec
  final def reset(continue: Continuation[State]): State = {
    continue match {
      case state: State =>
        state
      case io: IO[State] =>
        io.result()
      case tailCall =>
        reset(tailCall.step(identity[State]))
    }
  }

}

trait Continuation[+A] extends Any with Monad[A] {
  import Continuation.State
  type F[+A] = Continuation[A]
  type G[+A] = Continuation[A]

  final def run(continue: A => Continuation[State]): State = {
    Continuation.reset(step(continue))
  }

  def map[B](f: (A) => B): Continuation[B] = { continueB =>
    step { a => continueR =>
      continueB(f(a))
    }
  }

  def step(continue: A => Continuation[State]): Continuation[State]

  def flatMap[B](f: A => Continuation[B]): Continuation[B] = { continue =>
    step { a =>
      f(a).flatMap(continue)
    }
  }
}
