package com.thoughtworks.fastmonad

import scala.annotation.tailrec
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Monad[+A] extends Any { outer =>
//  F[+A] <: Monad[F, G, A], G[+A] >: F[A] <: Monad[F, G, A],
  type F[+A] <: Monad[A]
  type G[+A] <: Monad[A]
//  def map[B](f: A => B): Monad[B] = {
//    flatMap { a =>
//      val io: IO[B] = () => f(a)
//      io
//    }
//  }
  def map[B](f: A => B): F[B]
  def flatMap[B](f: A => G[B]): F[B]

}

//abstract class IO[A] extends Monad[A] {
//  def run(): A
//
//  def flatMap[B](f: (A) => Monad[B]) = {
//    f(run())
//  }
//} //package com.thoughtworks.fastmonad

//
///**
//  * @author 杨博 (Yang Bo)
//  */
//abstract class Continuation[A] extends Codensity[A] {
//  def listen(continue: A => Unit): Unit
//
////  def extract
//
//  def step(continue: (A) => Monad[Unit]): IO[Unit] = { () =>
//    listen { a =>
//      continue(a) match {
//        case io: IO[Unit] =>
//          io.run()
//        case codensity: Codensity[Unit] =>
//          codensity
//            .run { _: Unit =>
//              val ioUnit: IO[Unit] = (() => Unit)
//              ioUnit
//            }
//      }
//
//    }
//  }
//}
