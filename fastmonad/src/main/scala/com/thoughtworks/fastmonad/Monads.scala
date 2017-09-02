package com.thoughtworks.fastmonad

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Monads {

  type Type[+A] <: Monad[A]

  trait Monad[+A] {
    def map[B](f: A => B): Type[B] = {
      flatMap { a =>
        Monads.this.apply(f(a))
      }
    }

    def flatMap[B](f: A => Type[B]): Type[B]
  }

  def apply[A](a: A): Type[A]

}
