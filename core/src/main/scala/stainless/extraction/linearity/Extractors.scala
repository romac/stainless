/* Copyright 2009-2017 EPFL, Lausanne */

package stainless
package extraction
package linearity

trait Extractors { self: TypeChecker =>

  import trees._
  import symbols._

  object Wrapped {
    def unapply(expr: Expr): Option[Expr] = expr match {
      case FunctionInvocation(id, _, Seq(arg)) if id.toString.startsWith("wrapLinear<") => Some(arg)
      case _ => None
    }
  }

  object Discarded {
    def unapply(expr: Expr): Option[Expr] = expr match {
      // FIXME: Big Hack
      case w @ Wrapped(term) if expr.getType.toString == "Object" => Some(term)
      case _ => None
    }
  }

  object LinearTerm {
    def unapply(expr: Expr): Option[Expr] = expr match {
      case d @ Delinearize(v: Variable)        => Some(v.copy().setPos(d.getPos))
      case l @ Linearize(Operator(es, recons)) => Some(recons(es).setPos(l.getPos))
      case term if isLinear(term)              => Some(term)
      case _                                   => None
    }
  }

  def isLinear(tpe: Type): Boolean = tpe match {
    case LinearType(_) => true
    case _ => false
  }

  def isLinear(expr: Expr): Boolean = {
    isLinear(realType(expr))
  }

  def realType(expr: Expr): Type = expr match {
    case Delinearize(_) => LinearType(expr.getType)
    case Linearize(_)   => expr.getType
    case Wrapped(term)  => term.getType
    case _              => expr.getType
  }

  // FIXME: Huge hack
  def isThis(vd: ValDef): Boolean = {
    vd.id.toString == "thiss"
  }

  object ExThis {
    def unapply(vd: ValDef): Option[ValDef] = {
      if (isThis(vd)) Some(vd) else None
    }
  }

  object ExThisVar {
    def unapply(variable: Variable): Option[Variable] = {
      if (isThis(variable.toVal)) Some(variable) else None
    }
  }

  // FIXME: Big hack
  object MethodInvocation {
    private def isMethodInvocation(fi: FunctionInvocation): Boolean = {
      fi.tfd.params.headOption.map(isThis).getOrElse(false)
    }

    def unapply(expr: Expr): Option[(FunctionInvocation, Identifier, Seq[Type], Expr, Seq[Expr])] = expr match {
      case fi @ FunctionInvocation(id, tps, thiss +: args) if isMethodInvocation(fi) =>
        Some(fi, id, tps, thiss, args)
      case _ =>
        None
    }
  }

  object LinearMethodInvocation {
    private def isLinearMethodInvocation(fi: FunctionInvocation): Boolean = {
      fi.tfd.fd.flags.contains(Linear)
    }

    def unapply(expr: Expr): Option[(FunctionInvocation, Identifier, Seq[Type], Expr, Seq[Expr])] = expr match {
      case MethodInvocation(fi, id, tps, thiss, args) if isLinearMethodInvocation(fi) =>
        Some(fi, id, tps, thiss, args)
      case _ =>
        None
    }
  }

}
