/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package throwing

trait ExceptionLifting
  extends oo.SimplePhase
     with SimplyCachedFunctions
     with IdentitySorts
     with oo.IdentityClasses { self =>

  val s: Trees
  val t: Trees
  import s._

  override protected def getContext(symbols: s.Symbols) = new TransformerContext(symbols)

  protected class TransformerContext(symbols: s.Symbols) extends oo.TreeTransformer {
    override final val s: self.s.type = self.s
    override final val t: self.t.type = self.t
    import s._

    implicit private val syms = symbols

    private def getExceptionClass: Option[ClassDef] =
      symbols.lookup.get[ClassDef]("stainless.lang.Exception")

    private def getExceptionType: Option[ClassType] =
      getExceptionClass.map(_.typed.toType)

    val resultClass = {
      new ClassDef(
        FreshIdentifier("Result"),
        Seq(TypeParameterDef.fresh("E"), TypeParameterDef.fresh("T")),
        Seq.empty,
        Seq.empty,
        Seq(Synthetic, IsAbstract, IsSealed)
      )
    }

    val okClass = {
      val tparams = Seq(
        TypeParameterDef.fresh("E"),
        TypeParameterDef.fresh("T")
      )

      new ClassDef(
        FreshIdentifier("Ok"),
        tparams,
        Seq(resultClass.typed(tparams.map(_.tp)).toType),
        Seq(ValDef.fresh("value", tparams.last.tp)),
        Seq(Synthetic)
      )
    }

    val errClass = {
      val tparams = Seq(
        TypeParameterDef.fresh("E"),
        TypeParameterDef.fresh("T")
      )

      new ClassDef(
        FreshIdentifier("Err"),
        tparams,
        Seq(resultClass.typed(tparams.map(_.tp)).toType),
        Seq(ValDef.fresh("error", tparams.head.tp)),
        Seq(Synthetic)
      )
    }

    private def lift(fd: FunDef): FunDef = {
      val exceptionType = getExceptionType.get

      val tps = Seq(exceptionType, fd.returnType)
      val returnType = resultClass.typed(tps).toType

      def left(e: Expr) = ClassConstructor(errClass.typed(tps).toType, Seq(e))
      def right(e: Expr) = ClassConstructor(okClass.typed(tps).toType, Seq(e))

      val fullBody = exprOps.preMap {
        case Throw(expr) => Some(Return(left(expr)))
        case Return(expr) => Some(Return(right(expr)))
        case Block(stmts, Return(res)) => Some(Block(stmts, right(res)))
        case Block(stmts, res) => Some(Block(stmts, right(res)))
        case _ => None
      } (fd.fullBody)

      fd.copy(
        fullBody = fullBody,
        returnType = returnType
      )
    }

    override def transform(fd: FunDef): t.FunDef = {
      val throws = exprOps.exists {
        case Throw(_) => true
        case _ => false
      } (fd.fullBody)

      println(s"${fd.id} -> $throws")

      if (throws) super.transform(lift(fd))
      else super.transform(fd)
    }
  }

  override protected def extractSymbols(context: TransformerContext, symbols: s.Symbols): t.Symbols = {
    val extra = Seq(context.resultClass, context.okClass, context.errClass)
    super.extractSymbols(context, symbols)
      .withClasses(extra.map(context.transform(_)))
  }
}

object ExceptionLifting {
  def apply(tr: Trees)(implicit ctx: inox.Context): ExtractionPipeline {
    val s: tr.type
    val t: tr.type
  } = new ExceptionLifting {
    override val s: tr.type = tr
    override val t: tr.type = tr
    override val context = ctx
  }
}
