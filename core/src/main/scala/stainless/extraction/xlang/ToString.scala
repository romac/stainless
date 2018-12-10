/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package xlang

import scala.language.existentials

trait ToString
  extends oo.SimplePhase
     with SimplyCachedFunctions
     with IdentitySorts
     with oo.IdentityClasses { self =>

  val s: Trees
  val t: self.s.type
  import s._

  override protected def getContext(symbols: Symbols) = new TransformerContext(symbols)

  protected class TransformerContext(symbols: s.Symbols) extends oo.TreeTransformer {
    override final val s: self.s.type = self.s
    override final val t: self.t.type = self.t

    private def toString(e: Expr): Expr = {
      symbols.lookupCustomToString(e.getType(symbols)) match {
        case Some(printer) =>
          val res = exprOps.freshenLocals(printer(e))
          exprOps.postTraversal(_.copiedFrom(e))(res)
          res
        case None =>
          context.reporter.fatalError(
            s"Failed to find suitable Show instance for type: ${e.getType(symbols).asString}")
      }
    }

    override def transform(expr: Expr): Expr = expr match {
      case s.ToString(e, _) => super.transform(toString(e))
      case _ => super.transform(expr)
    }
  }
}

object ToString {
  def apply(trees: xlang.Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new ToString {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  }
}
