/* Copyright 2009-2018 EPFL, Lausanne */

package stainless.lang
import stainless.math

import stainless.annotation._

object Printers {

  @keep @library
  def BigIntToString(b: BigInt): String = {
    def digitToString(b: BigInt): String = b match {
      case _ if b == 0 => "0"
      case _ if b == 1 => "1"
      case _ if b == 2 => "2"
      case _ if b == 3 => "3"
      case _ if b == 4 => "4"
      case _ if b == 5 => "5"
      case _ if b == 6 => "6"
      case _ if b == 7 => "7"
      case _ if b == 8 => "8"
      case _ if b == 9 => "9"
    }
    var n = if (b < 0) -b else b
    var d: BigInt = 0
    var res = ""
    while (n > 9) {
      d = n % 10
      n = n / 10
      res = digitToString(d) + res
    }
    res = digitToString(n) + res
    if (b < 0) "-" + res else res
  }

  @keep @library
  def IntToString(b: Int): String = {
    def digitToString(b: Int): String = {
      require(b >= 0 && b < 10)
      b match {
        case 0 => "0"
        case 1 => "1"
        case 2 => "2"
        case 3 => "3"
        case 4 => "4"
        case 5 => "5"
        case 6 => "6"
        case 7 => "7"
        case 8 => "8"
        case 9 => "9"
      }
    }
    var n = if (b < 0) -b else b
    var d = 0
    var res = ""
    while (n > 9) {
      d = n % 10
      n = n / 10
      res = digitToString(d) + res
    }
    res = digitToString(n) + res
    if (b < 0) "-" + res else res
  }

  @keep @library
  def BooleanToString(b: Boolean): String = {
    if (b) "true" else "false"
  }

  @keep @library
  def StringToString(s: String): String = {
    s
  }

  @keep @extern @pure @library
  def GenericToString[A](x: A): String = x.toString

}
