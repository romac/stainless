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

  def toIdentifier(expr: Expr): Identifier = expr match {
    case ClassConstructor(ct, Seq(name, globalId, id)) if ct.id == lib.IdentifierClass.id =>
      new Identifier(toString(name), toInt(globalId), toInt(id))

    case other => sys.error(s"splice.NotAnIdentifier: $other (${other.getClass})")
  }

  def toString(expr: Expr): String = expr match {
    case ClassConstructor(ct, Seq(StringLiteral(value))) if ct.id == lib.StringLiteralClass.id =>
      value.toString

    case other => sys.error(s"splice.NotAString: $other (${other.getClass})")
  }

  def toInt(expr: Expr): Int = expr match {
    case ClassConstructor(ct, Seq(bv: BVLiteral)) if ct.id == lib.IntLiteralClass.id =>
      bv.toBigInt.toInt

    case other => sys.error(s"splice.NotAString: $other (${other.getClass})")
  }

  def toType(expr: Expr): Type = expr match {
    case ClassConstructor(ct, Seq()) if ct.id == lib.IntTypeClass.id =>
      BVType(true, 32)

    case ClassConstructor(ct, Seq()) if ct.id == lib.BooleanTypeClass.id =>
      BooleanType()

    case ClassConstructor(ct, Seq()) if ct.id == lib.StringTypeClass.id =>
      StringType()

    case other => sys.error(s"splice.NotAType: $other (${other.getClass})")
  }

  def splice(expr: Expr): Expr = expr match {
    case Quote(quoted) =>
      quoted

    case FunctionInvocation(id, tps, args) if symbols.functions(id).flags contains Meta =>
      val result = eval(expr)
      splice(result)

    case ClassConstructor(ct, Seq(metaId, metaTpe)) if ct.id == lib.VariableClass.id =>
      val id = toIdentifier(eval(metaId))
      val tpe = toType(eval(metaTpe))
      Variable(id, tpe, Seq.empty)

    case ClassConstructor(ct, Seq(value)) if ct.id == lib.IntLiteralClass.id =>
      value

    case ClassConstructor(ct, Seq(value)) if ct.id == lib.BooleanLiteralClass.id =>
      value

    case ClassConstructor(ct, Seq(value)) if ct.id == lib.StringLiteralClass.id =>
      value

    case ClassConstructor(ct, Seq(lhs, rhs)) if ct.id == lib.PlusClass.id =>
      Plus(splice(lhs), splice(rhs))

    case ClassConstructor(ct, Seq(lhs, rhs)) if ct.id == lib.EqualsClass.id =>
      Equals(splice(lhs), splice(rhs))

    case ClassConstructor(ct, Seq(lhs, rhs)) if ct.id == lib.StringConcatClass.id =>
      StringConcat(splice(lhs), splice(rhs))

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
