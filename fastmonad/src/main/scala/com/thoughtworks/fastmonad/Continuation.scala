package com.thoughtworks.fastmonad
import com.thoughtworks.fastmonad.Continuation.AnyContinuation.AnyState

import scala.annotation.tailrec
import scala.concurrent.SyncVar
import scala.language.higherKinds
import scala.language.implicitConversions

object Continuation {

  trait Suspend[State <: Continuation[State, State], +A] extends Bind[State, A] {
    def step(): Continuation[State, A]

    @tailrec
    private def run(): Continuation[State, A] = {
      step() match {
        case suspend: Suspend[State, A] =>
          suspend.run()
        case last =>
          last
      }
    }
    final def foreach(continue: (A) => State): State = {
      run().foreach(continue)
    }

    final def step(continue: (A) => Continuation[State, State]): Continuation[State, State] = {
      run().step(continue)
    }

  }

  trait Return[State <: Continuation[State, State], +A] extends Continuation[State, A] {
    def result(): A

    final def map[B](f: (A) => B): Return[State, B] = { () =>
      f(result())
    }

    final def flatMap[B](f: (A) => Continuation[State, B]): Suspend[State, B] = { () =>
      f(result())
    }

    @inline
    final def step(continue: A => Continuation[State, State]): Continuation[State, State] = {
      continue(result())
    }

    @inline
    final def foreach(continue: A => State): State = {
      continue(result())
    }
  }

  @tailrec
  final def reset[State <: Continuation[State, State]](continue: Continuation[State, State]): State = {
    continue match {
      case io: Return[State, State] =>
        io.result()
      case tailCall =>
        reset(tailCall.step(identity[State]))
    }
  }

  trait Bind[State <: Continuation[State, State], +A] extends Continuation[State, A] {

    @inline
    final def map[B](f: (A) => B): Async[State, B] = { continueB =>
      foreach { a =>
        continueB(f(a))
      }
    }

    @inline
    final def flatMap[B](f: A => Continuation[State, B]): Shift[State, B] = { continue =>
      step { a =>
        f(a).flatMap(continue)
      }
    }

  }

  trait Async[State <: Continuation[State, State], +A] extends Bind[State, A] {

    @inline
    final def step(continue: (A) => Continuation[State, State]): Continuation[State, State] = {
      foreach { a =>
        Continuation.reset(continue(a))
      }
    }
  }

  trait Shift[State <: Continuation[State, State], +A] extends Bind[State, A] {

    @inline
    final def foreach(continue: A => State): State = {
      Continuation.reset(step(continue))
    }

  }

  type AnyContinuation[+A] = Continuation[AnyState, A]
  object AnyContinuation {

    final class AnyState private[AnyContinuation] () extends Return[AnyState, AnyState] {
      override def result(): AnyState = this
    }
    val AnyState = new AnyState

    implicit def ToAnyState[A](a: A): AnyState = AnyState

    trait Delay[+A] extends Return[AnyState, A]

    def delay[A](f: Delay[A]): AnyContinuation[A] = f

    def blockingAwait[A](continuation: AnyContinuation[A]): A = {
      val box: SyncVar[A] = new SyncVar
      continuation.foreach { (a: A) =>
        box.put(a)
      }
      box.take
    }

  }

}

trait Continuation[State <: Continuation[State, State], +A] extends Monad[A] {
  type UpperBound[+A] = Continuation[State, A]
  type LowerBound[+A] = Continuation[State, A]
  def foreach(continue: A => State): State
  def step(continue: A => Continuation[State, State]): Continuation[State, State]
}
