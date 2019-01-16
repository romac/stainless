/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package xlang

import scala.language.existentials

trait CheckEquality
  extends oo.SimplePhase
     with IdentityFunctions
     with IdentitySorts
     with oo.IdentityClasses { self =>

  val t: self.s.type
  import s._

  import context.reporter

  override protected def getContext(symbols: Symbols) = new TransformerContext(symbols)

  protected class TransformerContext(symbols: s.Symbols) extends oo.TreeTransformer {
    override final val s: self.s.type = self.s
    override final val t: self.t.type = self.t

    private[this] implicit val implicitSymbols = symbols

    private[this] def isFunctionType(tpe: Typed): Boolean = tpe.getType match {
      case ft: FunctionType => true
      case _ => false
    }

    private[this] def reject(e: Expr): Unit = {
      throw MissformedStainlessCode(e, "Equality tests between lambdas are not supported in Stainless")
    }

    def ensureEqualityDecidability(fd: FunDef): Unit = {
      exprOps.preTraversal {
        case e @ Equals(HasType(lhs, FunctionType(_, _)), rhs) => reject(e)
        case e @ Equals(lhs, HasType(rhs, FunctionType(_, _))) => reject(e)
        case _ => ()
      } (fd.fullBody)
    }
  }

  override protected def extractFunction(context: TransformerContext, fd: s.FunDef): t.FunDef = {
    context.ensureEqualityDecidability(fd)
    super.extractFunction(context, fd)
  }
}

object CheckEquality {
  def apply(trees: xlang.Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: trees.type
    val t: trees.type
  } = new CheckEquality {
    override val s: trees.type = trees
    override val t: trees.type = trees
    override val context = ctx
  }
}
