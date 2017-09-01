package com.thoughtworks.fastmonad

import scala.annotation.tailrec

/**
  * @author 杨博 (Yang Bo)
  */
trait Trampoline[+A] extends Monad[A] {

  import com.thoughtworks.fastmonad.Trampoline._

  abstract override def flatMap[B](f: (A) => Monad[B]): Monad[B] = {
    super.flatMap(TailCall(_, f))
  }

}

object Trampoline {

  final case class TailCall[A, B](a: A, fab: A => Monad[B]) extends Monad[B] {

    def step = fab(a)

    // TODO: workaround tail call error
    //    @tailrec
    def run(): Monad[B] = {
      step match {
        case continue: TailCall[_, B] =>
          continue.run()
        case last =>
          last
      }
    }

    def flatMap[C](fbc: B => Monad[C]): Monad[C] = {
      run().flatMap(fbc)
    }

  }

}
