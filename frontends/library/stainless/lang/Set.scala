/* Copyright 2009-2018 EPFL, Lausanne */

package stainless.lang
import stainless.collection._
import stainless.annotation._

object Set {
  @library @inline
  def empty[T] = Set[T]()

  @ignore
  def apply[T](elems: T*) = {
    new Set[T](scala.collection.immutable.Set[T](elems : _*))
  }

  @extern @library
  def mkString[A](map: Set[A], infix: String, fA : A => String) = {
    map.theSet.map(fA).toList.sorted.mkString(infix)
  }
}

@ignore
case class Set[T](val theSet: scala.collection.immutable.Set[T]) {
  @ignore def +(a: T): Set[T] = new Set[T](theSet + a)
  @ignore def ++(a: Set[T]): Set[T] = new Set[T](theSet ++ a.theSet)
  @ignore def -(a: T): Set[T] = new Set[T](theSet - a)
  @ignore def --(a: Set[T]): Set[T] = new Set[T](theSet -- a.theSet)
  @ignore def size: BigInt = theSet.size
  @ignore def contains(a: T): Boolean = theSet.contains(a)
  @ignore def isEmpty: Boolean = theSet.isEmpty
  @ignore def subsetOf(b: Set[T]): Boolean = theSet.subsetOf(b.theSet)
  @ignore def &(a: Set[T]): Set[T] = new Set[T](theSet & a.theSet)

  // @extern @pure def toList: List[T] = {
  //   theSet.toList.foldRight(Nil[T](): List[T])(Cons(_, _))
  // } ensuring { _.content == this }
}

