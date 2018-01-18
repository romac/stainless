/* Copyright 2009-2017 EPFL, Lausanne */

package stainless
package extraction
package linearity

import inox.utils.Position

trait TypeChecker extends Extractors {

  val trees: Trees
  implicit val symbols: trees.Symbols

  import trees._
  import symbols._

  private var errors: List[LinearityError] = Nil

  def check(fd: FunDef): Either[Seq[LinearityError], Unit] = {
    if (fd.flags.contains("library") || fd.flags.contains("unchecked")) {
      return Right(())
    }

    fd.params foreach {
      case ExThis(vd) if fd.flags contains Linear =>
        checkUsedOnce(vd.copy(tpe = LinearType(vd.tpe)), fd.fullBody)
      case vd =>
        checkUsedOnce(vd, fd.fullBody)
    }

    val isLinearMethod = {
      fd.flags.contains(Linear) && fd.params.nonEmpty && isThis(fd.params.head)
    }

    val (specs, body) = exprOps.deconstructSpecs(fd.fullBody)
    body foreach { b =>
      check(b)(Delta(isLinearMethod))
    }

    errors match {
      case Nil => Right(())
      case _   => Left(errors.distinct.sortBy(_.pos))
    }
  }

  private def error(err: LinearityError): Unit = {
    errors = err :: errors
  }

  private def countOccurrences(vd: ValDef, expr: Expr): Int = expr match {
    // FIXME: Big hack
    case v: Variable if vd.id.toString == v.id.toString =>
      1

    case ADTSelector(adt, selector) if vd.id == selector =>
      countOccurrences(vd, adt) + 1

    case IfExpr(cnd, thn, els) =>
      val cndOcc = countOccurrences(vd, cnd)
      val thnOcc = countOccurrences(vd, thn)
      val elsOcc = countOccurrences(vd, els)
      cndOcc + Math.min(thnOcc, elsOcc)

    case MatchExpr(scrutinee, cases) =>
      val scrutOcc = countOccurrences(vd, scrutinee)
      val casesOcc = cases map { case MatchCase(pattern, optGuard, rhs) =>
        val optOcc = optGuard.map(opt => countOccurrences(vd, opt)).getOrElse(0)
        val rhsOcc = countOccurrences(vd, rhs)
        optOcc + rhsOcc
      }

      scrutOcc + casesOcc.foldLeft(1)(Math.min)

    case Operator(es, _) =>
      es.map(e => countOccurrences(vd, e)).sum
  }

  private def containsLinearFields(adt: TypedADTDefinition): Boolean = adt match {
    case cons: TypedADTConstructor => cons.fieldsTypes.exists(isLinear)
    case sort: TypedADTSort        => sort.constructors.exists(containsLinearFields)
  }

  private def checkUsedOnce(vd: ValDef, body: Expr): Unit = vd.getType match {
    case adt: ADTType if containsLinearFields(adt.getADT) =>
      error(NonLinearTypeContainsLinearFields(adt.getADT, vd.getPos))

    case LinearType(adt: ADTType) if adt.getADT.isInstanceOf[TypedADTConstructor] =>
      val occ = countOccurrences(vd, body)
      if (occ < 1) error(UnusedLinearVariable(vd, vd.getPos))

      val cons @ TypedADTConstructor(_, _) = adt.getADT

      cons.fields foreach { field =>
        if (isLinear(field.tpe)) {
          // FIXME: fields introduced in pattern matches should be counted too
          val occ = countOccurrences(field, body)
          if (occ < 1) error(UnusedLinearField(field, vd.getPos))
        }
      }

    case tpe if isLinear(tpe) =>
      val occ = countOccurrences(vd, body)
      if (occ < 1) error(UnusedLinearVariable(vd, vd.getPos))

    case _ => ()
  }

  private def isValidUse(expr: Expr, tpe: Type): Boolean = {
    !isLinear(expr) || isLinear(tpe)
  }

  private def checkValidUse(expr: Expr, tpe: Type, usage: Expr): Boolean = {
    val isValid = isValidUse(expr, tpe)
    if (!isValid) error(NonLinearTermUse(expr, usage.getPos))
    isValid
  }

  private def checkKinds(subst: Map[TypeParameter, Type]): Unit = {
    subst.filter(p => isLinear(p._2)) foreach { case (tp, ty) =>
      error(CannotInstantiateNonLinearTypeParam(tp, ty))
    }
  }

  private def checkKinds(tpe: ADTType): Unit = {
    val tps = tpe.getADT.definition.typeArgs
    val subst = tps.zip(tpe.tps).toMap.filter(tt => tt._1 != tt._2)
    checkKinds(subst)
  }

  private def check(expr: Expr)(delta: Delta): Delta = expr match {
    // TODO: Custom error message with FunDef
    case ExThisVar(thiss) if delta.isLinearMethod && delta.isUsed(thiss) =>
      error(LinearTermAlreadyUsed(thiss, delta usageOf thiss))
      delta

    case ExThisVar(thiss) if delta.isLinearMethod =>
      delta + (thiss -> thiss.getPos)

    case LinearTerm(term) if delta isUsed term =>
      error(LinearTermAlreadyUsed(term, delta usageOf term))
      delta

    case Discarded(LinearTerm(term)) =>
      error(DiscardedLinearTerm(term))
      check(term)(delta)

    case Linearize(term, _) =>
      check(term)(delta) + (term -> term.getPos)

    case LinearTerm(term) =>
      delta + (term -> term.getPos)

    case let @ Let(vd, value, body) =>
      checkValidUse(value, vd.tpe, let)
      checkUsedOnce(vd, body)

      val bindDelta = value match {
        case v: Variable => delta + (v -> let.getPos)
        case expr        => check(expr)(delta)
      }

      check(body)(bindDelta)

    case LinearTerm(l @ Lambda(args, body)) =>
      args foreach { arg => checkUsedOnce(arg, body) }
      check(body)(delta)

    case l @ Lambda(args, body) =>
      val linFV = exprOps.variablesOf(l).filter(isLinear(_))
      if (!linFV.isEmpty) {
        error(CannotCaptureLinearVar(l, linFV.head))
      }

      args foreach { arg =>
        checkUsedOnce(arg, body)
      }

      check(body)(delta)

    case LinearMethodInvocation(fi, id, _, thiss, args) if isLinear(thiss) =>
      checkKinds(fi.tfd.tpSubst)

      fi.tfd.params.zip(args) foreach { case (vd, arg) =>
        checkValidUse(arg, vd.tpe, fi)
      }

      check(thiss)(delta) >> checks(args)

    case MethodInvocation(fi, id, _, thiss, args) if isLinear(thiss) =>
      error(NonLinearMethodInvocation(thiss, fi, fi.tfd.fd))

      checkKinds(fi.tfd.tpSubst)

      fi.tfd.params.zip(args) foreach { case (vd, arg) =>
        checkValidUse(arg, vd.tpe, fi)
      }

      check(thiss)(delta) >> checks(args)

    case fi @ FunctionInvocation(id, tps, args) =>
      checkKinds(fi.tfd.tpSubst)

      fi.tfd.params.zip(args) foreach { case (vd, arg) =>
        checkValidUse(arg, vd.tpe, fi)
      }

      checks(args)(delta)

    case adt @ ADT(adtTpe, args) =>
      checkKinds(adtTpe)

      val params = adtTpe.getADT.toConstructor.fields
      params.zip(args) foreach { case (vd, arg) =>
        checkValidUse(arg, vd.getType, adt)
      }

      checks(args)(delta)

    case app @ Application(callee, args) =>
      val FunctionType(from, _) = callee.getType

      from.zip(args) foreach { case (tpe, arg) =>
        checkValidUse(arg, tpe, app)
      }

      check(callee)(delta) >> checks(args)

    case m @ MatchExpr(scrutinee, cases) =>
      cases foreach { case c =>
        c.pattern.binders.foreach { vd =>
          // FIXME: dummy tree just to put together the guard and the body of the case,
          //        in order to check occurrences for both in one go
          val dummy = c.optGuard.map(g => And(g, c.rhs).setPos(c)).getOrElse(c.rhs)
          checkUsedOnce(vd, dummy)
        }
      }

      check(scrutinee)(delta) >> checks(cases.flatMap(_.expressions))

    case IfExpr(cnd, thn, els) =>
      val afterCnd = check(cnd)(delta)
      val afterThn = check(thn)(afterCnd)
      val afterEls = check(els)(afterCnd)
      afterEls merge afterThn

    case Operator(es, _) =>
      checks(es)(delta)
  }

  private def checks(exprs: Seq[Expr])(delta: Delta): Delta = exprs match {
    case Seq()   => delta
    case e +: es => check(e)(delta) >> checks(es)
  }

  private case class Delta(
    isLinearMethod: Boolean,
    usedTerms: Map[Expr, Position] = Map.empty
  ) {

    def +(used: (Expr, Position)): Delta = {
      val (expr, pos) = used
      if (!usedTerms.contains(expr)) {
        this.copy(usedTerms = usedTerms + (expr -> pos))
      } else {
        this
      }
    }

    def isUsed(expr: Expr): Boolean = {
      usedTerms contains expr
    }

    def usageOf(expr: Expr): Position = {
      require(isUsed(expr))
      usedTerms(expr)
    }

    def merge(that: Delta): Delta = {
      this.copy(
        usedTerms = that.usedTerms.foldLeft(this.usedTerms)(_ + _)
      )
    }

    def >>(f: Delta => Delta): Delta = {
      f(this)
    }
  }

}

object TypeChecker {
  def apply(t: Trees)(implicit syms: t.Symbols): TypeChecker { val trees: t.type } = {
    new TypeChecker {
      val trees: t.type = t
      implicit val symbols: t.Symbols = syms
    }
  }
}
