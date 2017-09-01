package com.thoughtworks.fastmonad

/**
  * @author 杨博 (Yang Bo)
  */
abstract class Continuation[Unit, A] extends Codensity[Unit, A] {
  def listen(continue: A => Unit): Unit

//  def extract

  def run(continue: (A) => Monad[Unit]): IO[Unit] = { () =>
    listen { a =>
      continue(a)
    }
  }
}

