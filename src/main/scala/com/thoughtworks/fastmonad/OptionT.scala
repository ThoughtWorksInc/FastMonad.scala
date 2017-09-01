package com.thoughtworks.fastmonad

/**
  * @author 杨博 (Yang Bo)
  */
final case class OptionT[A](run: Monad[Option[A]]) extends AnyVal with Monad[A] {

  override def flatMap[B](f: (A) => Monad[B]): Monad[B] = {
    val optionBMonad: Monad[Option[B]] = run.flatMap {
      case Some(a) =>
        f(a).map(Some(_))
      case None =>
        OptionT.NoneCache
    }
    OptionT(optionBMonad)
  }

}

object OptionT {
  private val NoneCache: IO[None.type] = { () =>
    None
  }
}
