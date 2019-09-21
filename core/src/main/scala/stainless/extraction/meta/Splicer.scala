/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package meta

import inox.evaluators.EvaluationResults._

trait Splicer { self =>
  val trees: Trees
  import trees._

  implicit val symbols: trees.Symbols
  implicit val context: inox.Context

  val program = new inox.Program {
    val trees: self.trees.type = self.trees
    val symbols: self.symbols.type = self.symbols
  }

  val lib = meta.Library(trees)(symbols)
  val quoter = meta.Quoter(trees)(symbols)
  val evaluator = RecursiveEvaluator(program, context)

  def eval(expr: Expr): Expr = {
    evaluator.eval(expr) match {
      case Successful(value)     => value
      case RuntimeError(error)   => sys.error(error)
      case EvaluatorError(error) => sys.error(error)
    }
  }

  def splice(expr: Expr): Expr = expr match {
    case Quote(quoted) =>
      quoted

    case FunctionInvocation(id, tps, args) if symbols.functions(id).flags contains Meta =>
      val result = eval(expr)
      splice(result)

    case ClassConstructor(ct, args) if ct.id == lib.IntLiteralClass.id =>
      args.head

    case ClassConstructor(ct, args) if ct.id == lib.BooleanLiteralClass.id =>
      args.head

    case ClassConstructor(ct, Seq(lhs, rhs)) if ct.id == lib.PlusClass.id =>
      Plus(splice(lhs), splice(rhs))

    case ClassConstructor(ct, Seq(lhs, rhs)) if ct.id == lib.EqualsClass.id =>
      Equals(splice(lhs), splice(rhs))

    case ClassConstructor(ct, Seq(lhs, rhs)) if ct.id == lib.AssertClass.id =>
      Assert(splice(lhs), None, splice(rhs))

    case other => sys.error(s"splice.TODO: $other (${other.getClass})")
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
