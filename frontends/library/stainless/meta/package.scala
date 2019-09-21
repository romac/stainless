/* Copyright 2009-2019 EPFL, Lausanne */

package stainless

import stainless.lang._
import stainless.annotation._

import scala.language.implicitConversions

package object meta {

  object api {

    @library
    sealed abstract class Type

    @library
    object Type {

      @library
      case object Int extends Type

      @library
      case object Boolean extends Type
    }

    @library
    sealed abstract class Expr[A] {
      def getType: Type
    }

    object Expr {
      @library
      case class IntLiteral(value: Int) extends Expr[Int] {
        def getType = Type.Int
      }

      @library
      case class BooleanLiteral(value: Boolean) extends Expr[Boolean] {
        def getType = Type.Boolean
      }

      @library
      case class Plus(lhs: Expr[Int], rhs: Expr[Int]) extends Expr[Int] {
        def getType = Type.Int
      }

      @library
      case class Equals[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[Boolean] {
        def getType = Type.Boolean
      }

      @library
      case class Assert[A](pred: Expr[Boolean], body: Expr[A]) extends Expr {
        def getType = body.getType
      }
    }

    @ignore
    implicit def splice[A](expr: Expr[A]): A = ???

    @ignore
    implicit def quote[A](a: A): Expr[A] = ???
  }
}
