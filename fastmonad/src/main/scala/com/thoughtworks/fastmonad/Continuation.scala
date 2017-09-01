package com.thoughtworks.fastmonad

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.SyncVar
import scala.language.higherKinds
import scala.language.implicitConversions

object Continuation {

  trait State extends Continuation[State]

  abstract class Pure[+A] extends Delay[A] {
    def result: A
  }

  abstract class Suspend extends Bind[State] {
    def next(): Continuation[State]

    override def foreach(continue: (State) => State): State = {
      next().foreach(continue)
    }

    override def step(continue: (State) => Continuation[State]): Continuation[State] = {
      next().step(continue)
    }
  }

  abstract class Delay[+A] extends Continuation[A] {
    def result(): A

    final def map[B](f: (A) => B): Delay[B] = { () =>
      f(result())
    }

    final def flatMap[B](f: (A) => Continuation[B]): Async[B] = { continue =>
      f(result()).foreach(continue)
    }

    // @inline
    final def step(continue: A => Continuation[State]): Continuation[State] = {
      // TODO: tail call
      continue(result())
    }

    // @inline
    final def foreach(continue: A => State): State = {
      // TODO: tail call
      continue(result())
    }
  }

  @tailrec
  final def reset(continue: Continuation[State]): State = {
    continue match {
      case pure: Pure[State] =>
        pure.result
      case tailCall =>
        reset(tailCall.step(identity[State]))
    }
  }

  abstract class Bind[+A] extends Continuation[A] {

    // @inline
    final def map[B](f: (A) => B): Async[B] = { continueB =>
      foreach { a =>
        continueB(f(a))
      }
    }

    // @inline
    final def flatMap[B](f: A => Continuation[B]): Shift[B] = { continue =>
      step { a =>
        f(a).flatMap { b =>
          val suspend: Suspend = { () =>
            continue(b)
          }
          suspend
        }
      }

    }

  }

  abstract class Async[+A] extends Bind[A] {

    // @inline
    final def step(continue: (A) => Continuation[State]): Continuation[State] = {
      foreach { a =>
        Continuation.reset(continue(a))
      }
    }
  }

  abstract class Shift[+A] extends Bind[A] {

    // @inline
    final def foreach(continue: A => State): State = {
      Continuation.reset(step(continue))
    }

  }
//
//  type AnyContinuation[+A] = Continuation[AnyState, A]
//  object AnyContinuation {
//
//    final class AnyState private[AnyContinuation] () extends Pure[  AnyState] {
//      override def result = this
//    }
//
//    val AnyState = new AnyState
//
//    implicit def ToAnyState[A](a: A): AnyState = AnyState
//
//    def delay[A](f: Delay[AnyState, A]): AnyContinuation[A] = f
//
//    final def blockingAwait[A](continuation: AnyContinuation[A]): A = {
//      val box: SyncVar[A] = new SyncVar
//      continuation.foreach { (a: A) =>
//        box.put(a)
//      }
//      box.take
//    }
//
//  }

}

abstract class Continuation[+A] extends Monad[A] {
  import Continuation._
  type UpperBound[+A] = Continuation[A]
  type LowerBound[+A] = Continuation[A]
  def foreach(continue: A => State): State
  def step(continue: A => Continuation[State]): Continuation[State]
}
