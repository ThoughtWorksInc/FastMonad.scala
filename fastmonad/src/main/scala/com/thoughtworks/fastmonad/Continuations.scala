package com.thoughtworks.fastmonad

import scala.concurrent.SyncVar

/**
  * @author 杨博 (Yang Bo)
  */
trait Continuations[Result] extends Monads {

  def apply[A](a: A): Delay[A] = { () =>
    a
  }

  def delay[A](delay: Delay[A]): Delay[A] = delay

  type Type[+A] = Continuation[A]

  trait Continuation[+A] extends Monad[A] {

    def foreach(continue: A => Result): Result
    def suspend(continue: A => () => Result): () => Result

  }

  abstract class Shift[+A] extends Continuation[A] {

    override def map[B](f: (A) => B): Shift[B] = { continue =>
      Shift.this.suspend { a =>
        { () =>
          continue(f(a))
        }: Trampoline[Result]
      }
    }

    def flatMap[B](f: (A) => Continuation[B]): Shift[B] = { continue =>
      Shift.this.suspend { a =>
        { () =>
          f(a).suspend(continue)
        }: Trampoline[Result]
      }

    }

    def foreach(continue: (A) => Result): Result = {
      suspend { a => () =>
        continue(a)
      }()
    }

  }

  abstract class Async[+A] extends Type[A] {

    def suspend(continue: A => () => Result): () => Result = { () =>
      foreach { a =>
        continue(a)()
      }
    }

    override def map[B](f: (A) => B): Async[B] = { continue =>
      foreach { a =>
        continue(f(a))
      }
    }

    def flatMap[B](f: (A) => Type[B]): Shift[B] = { continue => () =>
      foreach { a =>
        f(a)
          .suspend { b =>
            continue(b)
          }()
      }
    }
  }

  trait Delay[+A] extends Type[A] {
    def result(): A

    def foreach(continue: A => Result): Result = {
      continue(result())
    }

    def suspend(continue: (A) => () => Result): () => Result = {
      continue(result())
    }

    override def map[B](f: (A) => B): Shift[B] = { continue =>
      { () =>
        continue(f(result()))
      }: Trampoline[Result]
    }

    def flatMap[B](f: (A) => Type[B]): Shift[B] = { continue =>
      val suspend: Trampoline[Result] = { () =>
        f(result()).suspend(continue)
      }
      suspend
    }
  }

}
object Continuations {
  object UnitContinuation extends Continuations[Unit] {

    implicit final class UnitContinuationOps[A](val underlying: Continuation[A]) extends AnyVal {
      def blockingAwait: A = {
        val box: SyncVar[A] = new SyncVar
        underlying.foreach { (a: A) =>
          box.put(a)
        }
        box.take
      }
    }

  }

  type UnitContinuation[+A] = UnitContinuation.Continuation[A]
}
