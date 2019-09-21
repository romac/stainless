/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package meta

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

  protected class TransformerContext(val symbols: s.Symbols) extends oo.TreeTransformer {
    override final val s: self.s.type = self.s
    override final val t: self.t.type = self.t

    implicit val syms: symbols.type = symbols

    val splicer = meta.Splicer(s)(symbols)
    val quote   = meta.Quoter(s)(symbols)
    val lib     = meta.Library(s)(symbols)

    def splice(metaExpr: Expr, tpe: Type): Expr = {
      val spliced = splicer.apply(metaExpr)

      if (!symbols.isSubtypeOf(spliced.getType, tpe)) {
        sys.error(s"Incompatible types: ${spliced.getType} <-> $tpe")
        return Error(tpe, s"Incompatible types: ${spliced.getType} <-> $tpe")
      }

      spliced
    }

    def exprType(tpe: Type): Type = tpe match {
      case ClassType(id, Seq(tpe)) if id == lib.ExprClass.id =>
        tpe

      case _ => sys.error("@meta methods must return an Expr[_]")
    }

    override def transform(e: Expr): t.Expr = e match {
      case fi @ FunctionInvocation(id, tps, args) if symbols.functions(id).flags contains s.Meta =>
        transform(splice(fi, symbols.functions(id).returnType))

      case Splice(metaExpr, tpe) =>
        transform(splice(metaExpr, tpe))

      case Quote(expr) => transform(quote(expr))

      case other => super.transform(e)
    }
  }
}

object Meta {
  def apply(trees: Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new Meta {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  }
}
