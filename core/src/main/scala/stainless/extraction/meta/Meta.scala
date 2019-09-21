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
  val t: s.type

  val trees: s.type = s
  import trees._

  override protected def getContext(symbols: Symbols) = new TransformerContext(symbols)

  protected class TransformerContext(val symbols: self.trees.Symbols) extends oo.TreeTransformer {
    override final val s: self.trees.type = self.trees
    override final val t: self.trees.type = self.trees

    implicit val syms: symbols.type = symbols

    val splicer = meta.Splicer(trees)(symbols)
    val quote   = meta.Quoter(trees)(symbols)
    val lib     = meta.Library(trees)(symbols)

    def splice(metaExpr: Expr, tpe: Type): Expr = {
      val spliced = splicer.splice(metaExpr)

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

    override def transform(e: Expr): Expr = e match {
      case fi @ FunctionInvocation(id, tps, args) if symbols.functions(id).flags contains s.Meta =>
        transform(splice(fi, symbols.functions(id).returnType))

      case Splice(metaExpr, tpe) =>
        transform(splice(metaExpr, tpe))

      case Quote(expr) => transform(quote(expr))

      case other => super.transform(e)
    }

    override def transform(fd: FunDef): FunDef = fd match {
      case fd if fd.flags contains trees.Meta => fd
      case fd => super.transform(fd)
    }
  }

  def isMeta(defn: Definition): Boolean = {
    defn.flags.contains(trees.Meta) ||
    defn.id.asInstanceOf[SymbolIdentifier].symbol.name.startsWith("stainless.meta.api")
  }

  override protected def registerFunctions(symbols: t.Symbols, functions: Seq[t.FunDef]): t.Symbols =
    symbols.withFunctions(functions.filterNot(isMeta))

  override protected def registerClasses(symbols: t.Symbols, classes: Seq[t.ClassDef]): t.Symbols =
    symbols.withClasses(classes.filterNot(isMeta))

}

object Meta {
  def apply(tr: Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: tr.type
    val t: tr.type
  } = new {
    val s: tr.type = tr
    val t: tr.type = tr
  } with Meta {
    override val context = ctx
  }
}
