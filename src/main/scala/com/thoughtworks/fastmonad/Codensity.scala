package com.thoughtworks.fastmonad

/**
  * @author 杨博 (Yang Bo)
  */
abstract class Codensity[R, A] extends Monad[A] {
  def run(continue: A => Monad[R]): Monad[R]

  def flatMap[B](f: A => Monad[B]): Codensity[R, B] = { continue =>
    run { a =>
      f(a).flatMap(continue)
    }
  }
}

