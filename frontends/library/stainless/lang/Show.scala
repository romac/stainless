/* Copyright 2009-2018 EPFL, Lausanne */

package stainless.lang

import stainless.collection._
import stainless.annotation._

@library @keep
abstract class Show[A] {
  def show(a: A): String
}

@library
object Show {
  @inline @library
  def apply[A](implicit ev: Show[A]): Show[A] = ev

  @library @keep implicit val showBoolean: Show[Boolean] = ShowBoolean()
  @library @keep implicit val showString: Show[String] = ShowString()
  @library @keep implicit val showBigInt: Show[BigInt] = ShowBigInt()
  @library @keep implicit val showLong: Show[Long] = ShowLong()
  @library @keep implicit val showInt: Show[Int] = ShowInt()
  @library @keep implicit val showShort: Show[Short] = ShowShort()
  @library @keep implicit val showByte: Show[Byte] = ShowByte()

  @library @keep implicit def showTuple2[A, B](implicit showA: Show[A], showB: Show[B]): Show[(A, B)] =
    ShowTuple2[A, B](showA, showB)

  @library @keep implicit def showList[A](implicit showA: Show[A]): Show[List[A]] =
    ShowList[A](showA)
}

@library @keep
case class ShowBoolean() extends Show[Boolean] {
  override def show(a: Boolean): String = Printers.BooleanToString(a)
}

@library @keep
case class ShowString() extends Show[String] {
  override def show(a: String): String = Printers.StringToString(a)
}

@library @keep
case class ShowBigInt() extends Show[BigInt] {
  override def show(a: BigInt): String = Printers.BigIntToString(a)
}

@library @keep
case class ShowLong() extends Show[Long] {
  override def show(a: Long): String = Printers.LongToString(a)
}

@library @keep
case class ShowInt() extends Show[Int] {
  override def show(a: Int): String = Printers.IntToString(a)
}

@library @keep
case class ShowShort() extends Show[Short] {
  override def show(a: Short): String = Printers.ShortToString(a)
}

@library @keep
case class ShowByte() extends Show[Byte] {
  override def show(a: Byte): String = Printers.ByteToString(a)
}

@library @keep
case class ShowTuple2[A, B](showA: Show[A], showB: Show[B]) extends Show[(A, B)] {
  override def show(a: (A, B)): String =
    Printers.Tuple2ToString(showA.show _, showB.show _, a)
}

@library @keep
case class ShowList[A](showA: Show[A]) extends Show[List[A]] {
  override def show(l: List[A]): String = l match {
    case Nil() => "Nil()"
    case Cons(x, xs) => showA.show(x) + " :: " + show(xs)
  }
}

