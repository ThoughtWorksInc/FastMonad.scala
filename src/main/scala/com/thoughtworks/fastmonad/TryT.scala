package com.thoughtworks.fastmonad

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author 杨博 (Yang Bo)
  */
case class TryT[A](run: Monad[Try[A]]) extends AnyVal with Monad[A] {

  override def flatMap[B](f: (A) => Monad[B]): Monad[B] = {
    val optionBMonad: Monad[Try[B]] = run.flatMap {
      case Success(a) =>
        try {
          f(a).map(Try(_))
        } catch {
          case NonFatal(e) =>
            TryT.ioError(e)
        }
      case Failure(e) =>
        TryT.ioError(e)
    }
    TryT(optionBMonad)
  }

}

object TryT {
  private def ioError[A](throwable: Throwable): IO[Failure[A]] = { () =>
    Failure(throwable)
  }

  def raiseError[A](throwable: Throwable) = TryT(ioError(throwable))
}
