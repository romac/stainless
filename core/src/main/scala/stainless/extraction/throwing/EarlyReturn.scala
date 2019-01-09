/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package throwing

trait EarlyReturn
  extends oo.SimplePhase
     with SimplyCachedFunctions
     with IdentitySorts
     with oo.IdentityClasses { self =>

  val s: Trees
  val t: oo.Trees

  override protected def getContext(symbols: s.Symbols) = new TransformerContext(symbols)

  protected class TransformerContext(symbols: s.Symbols) extends oo.TreeTransformer {
    override final val s: self.s.type = self.s
    override final val t: self.t.type = self.t
    import s._

    private def functionalize(fd: FunDef): FunDef = {
      val cpsBody = cps(fd.fullBody)
      println(cpsBody)
      fd.copy(fullBody = cpsBody)
    }

    private def cps(expr: Expr): Expr = {
      expr
    }

    override def transform(fd: FunDef): t.FunDef = {
      val hasReturn = exprOps.exists {
        case s.Return(_) => true
        case _ => false
      } (fd.fullBody)

      // println(s"${fd.id} -> $hasReturn")

      if (hasReturn) super.transform(functionalize(fd)) else super.transform(fd)
    }
  }
}

object EarlyReturn {
  def apply(ts: Trees, tt: oo.Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: ts.type
    val t: tt.type
  } = new EarlyReturn {
    override val s: ts.type = ts
    override val t: tt.type = tt
    override val context = ctx
  }
}
