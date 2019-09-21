/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package meta

trait Splicer {
  val trees: Trees
  import trees._

  implicit val symbols: trees.Symbols
  implicit val context: inox.Context

  val lib = meta.Library(trees)(symbols)

  def apply(expr: Expr): Expr = splice(expr)

  def splice(expr: Expr): Expr = expr match {
    case Quote(quoted) => quoted

    case ClassConstructor(ct, args) if ct.id == lib.IntLiteralClass.id =>
      args.head

    case ClassConstructor(ct, args) if ct.id == lib.PlusClass.id =>
      val Seq(lhs, rhs) = args
      Plus(splice(lhs), splice(rhs))

    case FunctionInvocation(id, tps, args) if symbols.functions(id).flags contains Meta =>
      val fd = symbols.functions(id)
      val res = exprOps.replaceFromSymbols(fd.params.zip(args).toMap, fd.fullBody)
      splice(exprOps.freshenLocals(res))

    case other => sys.error(s"TODO: $other (${other.getClass})")
  }

}

object Splicer {
  def apply(tr: Trees)(syms: tr.Symbols)(implicit ctx: inox.Context): Splicer {
    val trees: tr.type
  } = new {
    override val trees: tr.type = tr
    override val context = ctx
    override val symbols = syms
  } with Splicer
}
