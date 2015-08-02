package io.evolutionary

import scalaz._
import Scalaz._

import scalaz.stream._

import scala.language.higherKinds

package object twin {
  implicit class ProcessFromValue(p: Process.type) {
    def fromValue[T[_], S](s: => S)(implicit M: Monad[T]): Process[T, S] = Process.eval(Monad[T].point(s))
    def fromEffect[T[_], S](s: => Unit)(implicit M: Monad[T]): Process[T, S] = Process.eval_(Monad[T].point(s))
  }
}
