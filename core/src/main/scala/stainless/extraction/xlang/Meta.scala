/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package xlang

import scala.language.existentials

trait Meta
  extends oo.SimplePhase
     with SimplyCachedFunctions
     with SimplyCachedSorts
     with oo.IdentityTypeDefs
     with oo.SimplyCachedClasses { self =>

  val s: Trees
  val t: Trees

  import s._

  override protected def getContext(symbols: Symbols) = new TransformerContext(symbols)

  protected class TransformerContext(symbols: s.Symbols) extends oo.TreeTransformer {
    override final val s: self.s.type = self.s
    override final val t: self.t.type = self.t

    implicit private val syms: symbols.type = symbols

    val IntLiteralClass = symbols.lookup.get[ClassDef]("stainless.meta.api.IntLiteral").get
    val PlusClass = symbols.lookup.get[ClassDef]("stainless.meta.api.Plus").get

    def quote(expr: Expr): Expr = expr match {
      case Splice(expr, tpe) =>
        expr

      case BVLiteral(true, value, 32) =>
        ClassConstructor(IntLiteralClass.typed.toType, Seq(expr))

      case _ => sys.error("TODO")
    }

    def splice(expr: Expr): Expr = expr match {
      case Quote(quoted) => quoted

      case ClassConstructor(ct, args) if ct.id == IntLiteralClass.id =>
        args.head

      case ClassConstructor(ct, args) if ct.id == PlusClass.id =>
        val Seq(lhs, rhs) = args
        Plus(splice(lhs), splice(rhs))

      case FunctionInvocation(id, tps, args) => // TODO: Check meta annotation
        val fd = symbols.functions(id)
        val res = exprOps.replaceFromSymbols(fd.params.zip(args).toMap, fd.fullBody)
        splice(res)

      case other => sys.error(s"TODO: $other (${other.getClass})")
    }

    override def transform(e: Expr): t.Expr = e match {
      case Splice(metaExpr, tpe) =>
        val spliced = splice(metaExpr)

        val result =
          if (!symbols.isSubtypeOf(spliced.getType, tpe)) {
            sys.error(s"Incompatible types: ${spliced.getType} <-> $tpe")
            Error(tpe, s"Incompatible types: ${spliced.getType} <-> $tpe")
          } else {
            spliced
          }

        transform(result)

      case Quote(expr) => transform(quote(expr))

      case other => super.transform(e)
    }
  }
}

object Meta {
  def apply(trees: xlang.Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new Meta {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  }
}
