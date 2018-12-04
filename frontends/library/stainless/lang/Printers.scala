/* Copyright 2009-2018 EPFL, Lausanne */

package stainless.lang
import stainless.math

import stainless.annotation._

object Printers {

  @keep @library
  def digitToString(b: BigInt): String = {
    require(b >= 0 && b < 10)
    b match {
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
  }

  @keep @library
  def digitToString(b: Long): String = {
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

  @keep @library
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

  @keep @library
  def BigIntToString(b: BigInt): String = {
    def rec(n: BigInt, d: BigInt, acc: String): String = {
      if (n > 9) {
        val rem: BigInt = n % 10
        rec(n / 10, rem, digitToString(rem) + acc)
      } else {
        digitToString(n) + acc
      }
    }

    val res = rec(if (b < 0) -b else b, 0, "")
    if (b < 0) "-" + res else res
  }

  @keep @library
  def LongToString(b: Long): String = {
    def rec(n: Long, d: Long, acc: String): String = {
      if (n > 9) {
        val rem: Long = n % 10
        rec(n / 10, rem, digitToString(rem) + acc)
      } else {
        digitToString(n) + acc
      }
    }

    val res = rec(if (b < 0) -b else b, 0, "")
    if (b < 0) "-" + res else res
  }

  @keep @library
  def IntToString(b: Int): String = {
    def rec(n: Int, d: Int, acc: String): String = {
      if (n > 9) {
        val rem: Int = n % 10
        rec(n / 10, rem, digitToString(rem) + acc)
      } else {
        digitToString(n) + acc
      }
    }

    val res = rec(if (b < 0) -b else b, 0, "")
    if (b < 0) "-" + res else res
  }

  @keep @library
  def ShortToString(b: Short): String = {
    def rec(n: Int, d: Int, acc: String): String = {
      if (n > 9) {
        val rem = n % 10
        rec(n / 10, rem, digitToString(rem) + acc)
      } else {
        digitToString(n) + acc
      }
    }

    val res = rec(if (b < 0) -b else b, 0, "")
    if (b < 0) "-" + res else res
  }

  @keep @library
  def ByteToString(b: Byte): String = {
    def rec(n: Int, d: Int, acc: String): String = {
      if (n > 9) {
        val rem = n % 10
        rec(n / 10, rem, digitToString(rem) + acc)
      } else {
        digitToString(n) + acc
      }
    }

    val res = rec(if (b < 0) -b else b, 0, "")
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

  @keep @library
  def Tuple2ToString[A, B](f: A => String, g: B => String, t: (A, B)) = {
    "(" + f(t._1) + "," + g(t._2) + ")"
  }

  @keep @extern @pure @library
  def GenericToString[A](x: A): String = x.toString

}
