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
      'BigInt  -> symbols.lookup.get[FunDef](prefix + "BigIntToString"),
      'Int     -> symbols.lookup.get[FunDef](prefix + "IntToString"),
      'Boolean -> symbols.lookup.get[FunDef](prefix + "BooleanToString"),
      'String  -> symbols.lookup.get[FunDef](prefix + "StringToString"),
      'Generic -> symbols.lookup.get[FunDef](prefix + "GenericToString")
    )

    def toString(e: Expr, id: scala.Symbol, tps: Seq[Type]): Expr =
      toStringFuns(id).get.typed(tps).applied(Seq(e)).copiedFrom(e)

    override def transform(e: Expr): Expr = e match {
      case s.ToString(e, StringType()) =>
        super.transform(toString(e, 'String, Seq.empty))

      case s.ToString(e, IntegerType()) =>
        super.transform(toString(e, 'BigInt, Seq.empty))

      case s.ToString(e, BooleanType()) =>
        super.transform(toString(e, 'Boolean, Seq.empty))

      case s.ToString(e, BVType(true, 32)) =>
        super.transform(toString(e, 'Int, Seq.empty))

      case s.ToString(e, tp: TypeParameter) =>
        super.transform(toString(e, 'Generic, Seq(tp)))

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
