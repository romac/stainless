/* Copyright 2009-2019 EPFL, Lausanne */

package stainless

import stainless.lang._
import stainless.annotation._

import scala.language.implicitConversions

package object meta {

  object api {

    @library
    case class Identifier(name: String, globalId: Int, id: Int)

    @library
    sealed abstract class Type

    @library
    object Type {

      @library
      case object Int extends Type

      @library
      case object Boolean extends Type

      @library
      case object String extends Type
    }

    @library
    sealed abstract class Expr[A] {
      def getType: Type
    }

    object Expr {
      @library
      case class Variable[A](id: Identifier, tpe: Type) extends Expr[A] {
        def getType = tpe
      }

      @library
      case class IntLiteral(value: Int) extends Expr[Int] {
        def getType = Type.Int
      }

      @library
      case class BooleanLiteral(value: Boolean) extends Expr[Boolean] {
        def getType = Type.Boolean
      }

      @library
      case class StringLiteral(value: String) extends Expr[String] {
        def getType = Type.String
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

      @library
      case class StringConcat(lhs: Expr[String], rhs: Expr[String]) extends Expr[String] {
        def getType = Type.String
      }
    }

    @ignore
    implicit def splice[A](expr: Expr[A]): A = ???

    @ignore
    implicit def quote[A](a: A): Expr[A] = ???

    @library
    case class Theorem private (toExpr: Expr[Boolean])

    @library
    sealed abstract class SolverResult
    object SolverResult {
      @library
      case class CounterExample(model: Map[Identifier, Expr[Any]]) extends SolverResult
      @library
      case class Valid(theorem: Theorem) extends SolverResult
      @library
      case object Unknown extends SolverResult
    }

    @ignore
    def find(query: Expr[Boolean]): SolverResult = ???

    @ignore
    implicit class BooleanOps(val prop: Boolean) extends AnyVal {
      def by(meta: Expr[Boolean] => Theorem): Boolean = ???
    }
  }
}

