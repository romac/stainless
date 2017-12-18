/* Copyright 2009-2017 EPFL, Lausanne */

package stainless

import stainless.lang._
import stainless.annotation._

import scala.language.implicitConversions

package object linear {

  @ignore
  class Linear[+A](_value: A) {
    def ! = _value
  }

  @ignore
  implicit def delinearize[A](lin: Linear[A]): A = lin.!

  @ignore
  implicit def linearize[A](value: A): Linear[A] = new Linear(value)

}
