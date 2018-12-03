/* Copyright 2009-2018 EPFL, Lausanne */

package stainless.lang

import stainless.annotation._

object Printers {

  @keep @library
  def BigIntToString(b: BigInt): String = {
    "42"
  }

  @keep @library
  def BooleanToString(b: Boolean): String = {
    if (b) "true" else "false"
  }

  @keep @library
  def StringToString(s: String): String = {
    s
  }

  @keep @library
  def IntToString(i: Int): String = {
    "42"
  }

  @keep @extern @pure @library
  def GenericToString[A](x: A): String = x.toString

}
