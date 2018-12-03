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

    implicit val syms = symbols

    val prefix = "stainless.lang.Printers."

    val toStringFuns = Map(
      'BigInt  -> symbols.lookup[FunDef](prefix + "BigIntToString"),
      'String  -> symbols.lookup[FunDef](prefix + "StringToString"),
      'Generic -> symbols.lookup[FunDef](prefix + "GenericToString")
    )

    override def transform(e: Expr): Expr = e match {
      case s.ToString(e, StringType()) =>
        super.transform(e)

      case s.ToString(e, tp: TypeParameter) =>
        toStringFuns('Generic).typed(Seq(tp)).applied(Seq(e)).copiedFrom(e)

      case _ => super.transform(e)
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
