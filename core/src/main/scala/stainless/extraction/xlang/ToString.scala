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

    implicit private val syms = symbols

    private def lookup(typeName: String): FunDef =
      symbols.lookup[FunDef](s"stainless.lang.Printers.${typeName}ToString")

    private sealed abstract class Printer {
      def applied(expr: Expr): Expr
      def partiallyApplied(args: Seq[Expr]): Lambda
      def typed(tps: Seq[Type] = Seq.empty): Printer
      def toLambda: Lambda = partiallyApplied(Seq.empty)
    }

    private sealed case class PrintLambda(lam: Lambda) extends Printer {
      override def typed(tps: Seq[Type] = Seq.empty): Printer = this
      override def applied(expr: Expr): Expr = exprOps.freshenLocals(lam.withParamSubst(Seq(expr), lam.body))
      override def partiallyApplied(args: Seq[Expr]): Lambda = {
        require(args.isEmpty)
        lam
      }
    }

    private sealed case class PrintFunction(tfd: TypedFunDef) extends Printer {
      override def typed(tps: Seq[Type] = Seq.empty): Printer = 
        if (tps.isEmpty) PrintFunction(tfd.fd.typed)
        else PrintFunction(tfd.fd.typed(tps))

      override def applied(expr: Expr): Expr =
        exprOps.freshenLocals(tfd.applied(Seq(expr)))

      override def partiallyApplied(args: Seq[Expr]): Lambda = {
        require(args.length <= tfd.params.length)
        val vds = tfd.params.drop(args.length).map(_.freshen)
        Lambda(vds, tfd.applied(args ++ vds.map(_.toVariable)))
      }
    }

    private def getPrinter(tpe: Type): Printer = tpe match {
      case tp: TypeParameter =>
        PrintFunction(lookup("Generic").typed(Seq(tp)))

      case BooleanType() =>
        PrintFunction(lookup("Boolean").typed)

      case IntegerType() =>
        PrintFunction(lookup("BigInt").typed)

      case BVType(true, 64) =>
        PrintFunction(lookup("Long").typed)

      case BVType(true, 32) =>
        PrintFunction(lookup("Int").typed)

      case BVType(true, 16) =>
        PrintFunction(lookup("Short").typed)

      case BVType(true, 8) =>
        PrintFunction(lookup("Byte").typed)

      case StringType() =>
        PrintFunction(lookup("String").typed)

      case TupleType(tps) =>
        val args = tps map (t => getPrinter(t).toLambda)
        val printer = lookup("Tuple" + tps.length).typed(tps)
        PrintLambda(PrintFunction(printer).partiallyApplied(args))
    }

    private def toString(e: Expr): Expr = {
      val res = exprOps.freshenLocals(getPrinter(e.getType).applied(e))
      exprOps.postTraversal(_.copiedFrom(e))(res)
      res
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
