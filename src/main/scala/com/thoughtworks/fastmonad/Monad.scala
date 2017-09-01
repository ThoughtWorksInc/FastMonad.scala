package com.thoughtworks.fastmonad

/**
  * @author 杨博 (Yang Bo)
  */
trait Monad[+A] extends Any {
  def map[B](f: A => B): Monad[B] = {
    flatMap { a =>
      val io: IO[B] = () => f(a)
      io
    }
  }

  def flatMap[B](f: A => Monad[B]): Monad[B]

}
