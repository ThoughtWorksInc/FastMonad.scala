package com.thoughtworks.fastmonad

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Monad[+A] extends Any { outer =>
  type UpperBound[+A] <: Monad[A]
  type LowerBound[+A] <: Monad[A]

  def map[B](f: A => B): UpperBound[B]
  def flatMap[B](f: A => LowerBound[B]): UpperBound[B]

}
