package com.thoughtworks.fastmonad

import com.thoughtworks.fastmonad.Continuations.UnitContinuation

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author 杨博 (Yang Bo)
  */
class TryTs(val underlyingCompanion: Monads) extends Monads {

  type Type[+A] = TryT[A]

  class TryT[+A](val run: underlyingCompanion.Type[Try[A]]) extends Monad[A] {
    override def map[B](f: (A) => B): TryT[B] = {
      new TryT(run.map { a =>
        a.map(f)
      })
    }

    def flatMap[B](f: (A) => TryT[B]): TryT[B] = {
      val underlying = run.flatMap {
        case Success(a) =>
          try {
            f(a).run
          } catch {
            case NonFatal(e) =>
              underlyingCompanion(Failure[B](e))
          }
        case Failure(e) =>
          underlyingCompanion(Failure[B](e))
      }
      new TryT(underlying)
    }
  }

  override def apply[A](a: A) = new TryT[A](underlyingCompanion(Success[A](a)))

}

object TryTs {

  object Task extends TryTs(UnitContinuation)
  type Task[+A] = Task.Type[A]

}
