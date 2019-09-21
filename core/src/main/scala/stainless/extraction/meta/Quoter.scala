/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package meta

trait Quoter {
  val trees: Trees
  import trees._

  implicit val symbols: trees.Symbols
  implicit val context: inox.Context

  val splice = meta.Splicer(trees)(symbols)
  val lib = meta.Library(trees)(symbols)

  def apply(expr: Expr): Expr = quote(expr)

  def quote(expr: Expr): Expr = expr match {
    case Splice(expr, tpe) =>
      expr

    case BVLiteral(true, value, 32) =>
      ClassConstructor(lib.IntLiteralClass.typed.toType, Seq(expr))

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
