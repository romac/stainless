/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package meta

trait Quoter {
  val trees: Trees
  import trees._

  implicit val symbols: trees.Symbols
  implicit val context: inox.Context

  val lib = meta.Library(trees)(symbols)

  def apply(expr: Expr): Expr = quote(expr)

  def quote(expr: Expr): Expr = expr match {
    case Splice(expr, tpe) =>
      expr

    case BVLiteral(true, _, 32) =>
      ClassConstructor(lib.IntLiteralClass.typed.toType, Seq(expr))

    case BooleanLiteral(_) =>
      ClassConstructor(lib.BooleanLiteralClass.typed.toType, Seq(expr))

    case Plus(lhs, rhs) =>
      ClassConstructor(lib.PlusClass.typed.toType, Seq(quote(lhs), quote(rhs)))

    case Equals(lhs, rhs) =>
      ClassConstructor(lib.EqualsClass.typed.toType, Seq(quote(lhs), quote(rhs)))

    case Assert(pred, msg, body) =>
      ClassConstructor(lib.AssertClass.typed.toType, Seq(quote(pred), quote(body)))

    case other => sys.error(s"TODO: $other (${other.getClass})")
  }

}

object Quoter {
  def apply(tr: Trees)(syms: tr.Symbols)(implicit ctx: inox.Context): Quoter {
    val trees: tr.type
  } = new {
    val trees: tr.type = tr
    val context = ctx
    val symbols = syms
    } with Quoter
}
