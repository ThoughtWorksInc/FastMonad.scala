package com.thoughtworks.fastmonad
import scala.annotation.tailrec

abstract class Trampoline[A] extends (() => A) {
  def step(): () => A

  @tailrec
  override final def apply(): A = {
    step() match {
      case tailCall: Trampoline[A] =>
        tailCall.apply()
      case notTailCall =>
        notTailCall()
    }
  }
}
