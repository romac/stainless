/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package meta

trait RecursiveEvaluator extends stainless.evaluators.RecursiveEvaluator {
  val program: Program { val trees: meta.Trees }

  import context._
  import program._
  import program.trees._
  import program.symbols._

  val quoter = meta.Quoter(trees)(symbols)

  override def e(expr: Expr)(implicit rctx: RC, gctx: GC): Expr = expr match {
    case Quote(quoted) =>
      quoter.quote(quoted)

    case ClassConstructor(ct, args) =>
      ClassConstructor(ct, args map e)

    case _ => super.e(expr)
  }
}

object RecursiveEvaluator {
  def apply(p: Program { val trees: Trees }, ctx: inox.Context): RecursiveEvaluator {
    val program: p.type
  } = {
    new {
      val program: p.type = p
      val context = ctx
    } with RecursiveEvaluator
      with inox.evaluators.HasDefaultGlobalContext
      with inox.evaluators.HasDefaultRecContext {
      val semantics = null // p.getSemantics(meta.metaSemantics.asInstanceOf)
    }
  }
}
