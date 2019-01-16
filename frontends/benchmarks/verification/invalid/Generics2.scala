/* Copyright 2009-2018 EPFL, Lausanne */

import stainless.lang._

object Generics2 {
  abstract class List[T]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case class Nil[B]() extends List[B]

  def size[T](l: List[T]): BigInt =
    (l match {
      case Nil() => BigInt(0)
      case Cons(h, t) => 1 + size(t)
    }) ensuring { _ >= 0 }

  def foo[T](l: List[T]): List[T] = {
    require(size(l) < 2)

    l
  }

  def bar(l: List[Int]) = {
    foo(l)
  }

}
