package com.thoughtworks.fastmonad
import scala.annotation.tailrec

object Continuation {

  trait IO[State <: Continuation[State, State], +Result] extends Continuation[State, Result] {
    def result(): Result
    final def step(continue: Result => Continuation[State, State]): Continuation[State, State] = {
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

    type Continuation[+Result] = com.thoughtworks.fastmonad.Continuation[NoState, Result]

    type IO[+Result] = com.thoughtworks.fastmonad.Continuation.IO[NoState, Result]
  }

  final case class NoState private[Continuation] () extends NoState.IO[NoState] {
    override def result(): NoState = this
  }

}

trait Continuation[State <: Continuation[State, State], +Result] extends Any with Monad[Result] {

  type UpperBound[+A] = Continuation[State, A]
  type LowerBound[+A] = Continuation[State, A]

  /** @usecase def onComplete(continue: A => State): State = ???
    */
  final def onComplete(continue: Result => Continuation[State, State]): State = {
    Continuation.reset(step(continue))
  }

  def map[B](f: (Result) => B): Continuation[State, B] = { continueB =>
    step { a => continueR =>
      continueB(f(a))
    }
  }

  def step(continue: Result => Continuation[State, State]): Continuation[State, State]

  def flatMap[B](f: Result => Continuation[State, B]): Continuation[State, B] = { continue =>
    step { a =>
      f(a).flatMap(continue)
    }
  }
}
