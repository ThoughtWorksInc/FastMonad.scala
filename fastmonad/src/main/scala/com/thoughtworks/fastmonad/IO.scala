//package com.thoughtworks.fastmonad
//
///**
//  * @author 杨博 (Yang Bo)
//  */
//abstract class IO[A] extends Monad[A] {
//  def run(): A
//
//  def flatMap[B](f: (A) => Monad[B]) = {
//    f(run())
//  }
//}
