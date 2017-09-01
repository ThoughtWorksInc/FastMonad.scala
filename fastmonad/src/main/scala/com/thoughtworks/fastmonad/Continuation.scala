package com.thoughtworks.fastmonad
import scala.annotation.tailrec

object Continuation {

  trait IO[State <: Continuation[State, State], +A] extends Continuation[State, A] {
    def result(): A
    final def step(continue: A => Continuation[State, State]): Continuation[State, State] = {
      continue(result())
    }
  }

  @tailrec
  final def reset[State <: Continuation[State, State]](continue: Continuation[State, State]): State = {
    continue match {
      case io: IO[State, State] =>
        io.result()
      case tailCall =>
        reset(tailCall.step(identity[State]))
    }
  }

  object NoState {
    private val Instance = new NoState

    def apply(): NoState = Instance

    type Continuation[+A] = com.thoughtworks.fastmonad.Continuation[NoState, A]

    type IO[+A] = com.thoughtworks.fastmonad.Continuation.IO[NoState, A]
  }

  final case class NoState private[Continuation] () extends NoState.IO[NoState] {
    override def result(): NoState = this
  }

}

trait Continuation[State <: Continuation[State, State], +A] extends Any with Monad[A] {

  type F[+A] = Continuation[State, A]
  type G[+A] = Continuation[State, A]

  /** @usecase def onComplete(continue: A => State): State = ???
    */
  final def onComplete(continue: A => Continuation[State, State]): State = {
    Continuation.reset(step(continue))
  }

  def map[B](f: (A) => B): Continuation[State, B] = { continueB =>
    step { a => continueR =>
      continueB(f(a))
    }
  }

  def step(continue: A => Continuation[State, State]): Continuation[State, State]

  def flatMap[B](f: A => Continuation[State, B]): Continuation[State, B] = { continue =>
    step { a =>
      f(a).flatMap(continue)
    }
  }
}
