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

  def quote(n: Int): Expr = {
    ClassConstructor(lib.IntLiteralClass.typed.toType, Seq(BVLiteral(true, n, 32)))
  }

  def quote(s: String): Expr = {
    ClassConstructor(lib.StringLiteralClass.typed.toType, Seq(StringLiteral(s)))
  }

  def quote(id: Identifier): Expr = {
    val metaName = quote(id.name)
    val metaGlobalId = quote(id.globalId)
    val metaId = quote(id.id)

    ClassConstructor(lib.IdentifierClass.typed.toType, Seq(metaName, metaGlobalId, metaId))
  }

  def quote(tpe: Type): Expr = tpe match {
    case BVType(true, 32) =>
      ClassConstructor(lib.IntTypeClass.typed.toType, Seq.empty)

    case BooleanType() =>
      ClassConstructor(lib.BooleanTypeClass.typed.toType, Seq.empty)

    case StringType() =>
      ClassConstructor(lib.StringTypeClass.typed.toType, Seq.empty)

    case other => sys.error(s"quoter.TODO: $other (${other.getClass})")
  }

  def quote(expr: Expr): Expr = expr match {
    case Splice(expr, tpe) =>
      expr

    case Variable(id, tpe, _) => // TODO: Flags
      ClassConstructor(lib.VariableClass.typed.toType, Seq(quote(id), quote(tpe)))

    case BVLiteral(true, _, 32) =>
      ClassConstructor(lib.IntLiteralClass.typed.toType, Seq(expr))

    case BooleanLiteral(_) =>
      ClassConstructor(lib.BooleanLiteralClass.typed.toType, Seq(expr))

    case StringLiteral(_) =>
      ClassConstructor(lib.StringLiteralClass.typed.toType, Seq(expr))

    case Plus(lhs, rhs) =>
      ClassConstructor(lib.PlusClass.typed.toType, Seq(quote(lhs), quote(rhs)))

    case Equals(lhs, rhs) =>
      ClassConstructor(lib.EqualsClass.typed.toType, Seq(quote(lhs), quote(rhs)))

    case StringConcat(lhs, rhs) =>
      ClassConstructor(lib.StringConcatClass.typed.toType, Seq(quote(lhs), quote(rhs)))

    case Assert(pred, msg, body) =>
      ClassConstructor(lib.AssertClass.typed.toType, Seq(quote(pred), quote(body)))

    case other => sys.error(s"quoter.TODO: $other (${other.getClass})")
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
