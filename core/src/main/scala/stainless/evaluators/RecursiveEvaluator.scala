/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package evaluators

trait RecursiveEvaluator extends inox.evaluators.RecursiveEvaluator {
  val program: Program

  import context._
  import program._
  import program.trees._
  import program.symbols._

  override def e(expr: Expr)(implicit rctx: RC, gctx: GC): Expr = expr match {
    case Require(pred, body) =>
      if (!ignoreContracts && e(pred) != BooleanLiteral(true))
        throw RuntimeError("Requirement did not hold @" + expr.getPos)
      e(body)

    case en @ Ensuring(body, pred) =>
      e(en.toAssert)

    case Assert(pred, err, body) =>
      if (!ignoreContracts && e(pred) != BooleanLiteral(true))
        throw RuntimeError(err.getOrElse("Assertion failed @" + expr.getPos))
      e(body)

    case MatchExpr(scrut, cases) =>
      val rscrut = e(scrut)
      cases.toStream.map(c => matchesCase(rscrut, c).map(c -> _)).find(_.nonEmpty) match {
        case Some(Some((c, mapping))) => e(c.rhs)(rctx.withNewVars(mapping), gctx)
        case _ => throw RuntimeError("MatchError: " + rscrut + " did not match any of the cases:\n" + cases)
      }

    case FiniteArray(elems, base) =>
      FiniteArray(elems.map(e), base)

    case LargeArray(elems, default, size, base) =>
      LargeArray(elems.map(p => p._1 -> e(p._2)), e(default), e(size), base)

    case ArraySelect(array, index) => (e(array), e(index)) match {
      case (FiniteArray(elems, base), Int32Literal(i)) =>
        if (i < 0 || i >= elems.size) throw RuntimeError("Index out of bounds @" + expr.getPos)
        elems(i)
      case (LargeArray(elems, default, Int32Literal(size), _), Int32Literal(i)) =>
        if (i < 0 || i >= size) throw RuntimeError("Index out of bounds @" + expr.getPos)
        elems.getOrElse(i, default)
    }

    case ArrayUpdated(array, index, value) => (e(array), e(index)) match {
      case (FiniteArray(elems, base), Int32Literal(i)) =>
        if (i < 0 || i >= elems.size) throw RuntimeError("Index out of bounds @" + expr.getPos)
        FiniteArray(elems.updated(i, e(value)), base)
      case (LargeArray(elems, default, s @ Int32Literal(size), base), Int32Literal(i)) =>
        if (i < 0 || i >= size) throw RuntimeError("Index out of bounds @" + expr.getPos)
        LargeArray(elems + (i -> e(value)), default, s, base)
    }

    case ArrayLength(array) => e(array) match {
      case FiniteArray(elems, _) => Int32Literal(elems.size)
      case LargeArray(_, _, s, _) => s
    }

    case ToString(value) => e(value) match {
      case lit: Literal[_] => StringLiteral(lit.value.toString)
      case adt: ADT =>
        val ADTType(_, tps) = adt.getType
        lookupCustomToString(adt.getType) match {
          case Some(fd) => e(fd.typed(tps).applied(Seq(adt)))
          case None => ToString(adt).copiedFrom(expr)
        }
        case other => ToString(other).copiedFrom(expr)
    }

    case Error(tpe, msg) =>
      throw RuntimeError("Error reached in evaluation: " + msg)

    case NoTree(tpe) =>
      throw RuntimeError("Reached empty tree in evaluation @" + expr.getPos)

    case Annotated(body, _) =>
      e(body)

    case _ => super.e(expr)
  }

  private def lookupCustomToString(tpe: Type): Option[FunDef] = {
    symbols.functions.values.find { fd =>
      fd.id.name == "toString" &&
      fd.params.size == 1 &&
      ((fd.params.head.getType, tpe) match {
        case (ADTType(id1, _), ADTType(id2, _)) => id1 == id2
        case _ => false
      })
    }
  }

  protected def matchesCase(scrut: Expr, caze: MatchCase)
                           (implicit rctx: RC, gctx: GC): Option[Map[ValDef, Expr]] = {

    def obind(ob: Option[ValDef], e: Expr): Map[ValDef, Expr] = {
      Map.empty[ValDef, Expr] ++ ob.map(_ -> e)
    }

    def matchesPattern(pat: Pattern, expr: Expr): Option[Map[ValDef, Expr]] = (pat, expr) match {
      case (WildcardPattern(ob), e) =>
        Some(obind(ob, e))

      case (ADTPattern(ob, id, tps, subs), ADT(id2, tps2, args)) =>
        if (id == id2 && tps == tps2) {
          val res = (subs zip args) map (p => matchesPattern(p._1, p._2))
          if (res.forall(_.isDefined)) {
            Some(obind(ob, expr) ++ res.flatten.flatten)
          } else {
            None
          }
        } else {
          None
        }

      case (TuplePattern(ob, subs), Tuple(args)) =>
        if (subs.size == args.size) {
          val res = (subs zip args) map (p => matchesPattern(p._1, p._2))
          if (res.forall(_.isDefined)) {
            Some(obind(ob, expr) ++ res.flatten.flatten)
          } else {
            None
          }
        } else {
          None
        }

      case (up @ UnapplyPattern(ob, rec, id, tps, subs), scrut) =>
        val eRec = rec map e
        val unapp = e(FunctionInvocation(id, tps, eRec.toSeq :+ scrut))
        e(up.isEmptyUnapplied(unapp)) match {
          case BooleanLiteral(false) =>
            val extracted = e(up.getUnapplied(unapp))
            val res = (subs zip unwrapTuple(extracted, subs.size)) map (p => matchesPattern(p._1, p._2))
            if (res.forall(_.isDefined)) {
              Some(obind(ob, expr) ++ res.flatten.flatten)
            } else {
              None
            }
          case BooleanLiteral(true) => None
          case other => throw EvalError(typeErrorMsg(other, BooleanType()))
        }

      case (LiteralPattern(ob, lit), e) if lit == e =>
        Some(obind(ob, e))

      case _ => None
    }

    matchesPattern(caze.pattern, scrut).flatMap { mapping =>
      caze.optGuard match {
        case Some(guard) =>
          if (e(guard)(rctx.withNewVars(mapping), gctx) == BooleanLiteral(true)) {
            Some(mapping)
          } else {
            None
          }
        case None =>
          Some(mapping)
      }
    }
  }
}

object RecursiveEvaluator {
  def apply(p: StainlessProgram, ctx: inox.Context): RecursiveEvaluator { val program: p.type } = {
    new {
      val program: p.type = p
      val context = ctx
    } with RecursiveEvaluator
      with inox.evaluators.HasDefaultGlobalContext
      with inox.evaluators.HasDefaultRecContext {

      val semantics = p.getSemantics
    }
  }
}
