/* Copyright 2009-2017 EPFL, Lausanne */

package stainless
package extraction
package linearity

import inox.Reporter
import inox.utils.Position

sealed abstract class LinearityError(val pos: Position) {
  def report(reporter: Reporter): Unit
}

case class LinearTermAlreadyUsed(term: Trees#Expr, usage: Position) extends LinearityError(term.getPos) {
  override def report(reporter: Reporter): Unit = {
    reporter.error(pos, s"linear term `$term` has already been used")
    reporter.info(usage, s"term used here:")
  }
}

case class NonLinearTermUse(term: Trees#Expr, usage: Position) extends LinearityError(term.getPos) {
  override def report(reporter: Reporter): Unit = {
    reporter.error(pos, s"linear term `$term` cannot be used in a non-linear context")
    reporter.info(usage, s"term used here in a non-linear context:")
  }
}

case class UnusedLinearVariable(vd: Trees#ValDef, override val pos: Position) extends LinearityError(pos) {
  override def report(reporter: Reporter): Unit = {
    reporter.error(pos, s"linear variable `${vd.id}` of type `${vd.tpe}` is never used")

    if (vd.getPos != pos) {
      reporter.info(vd.getPos, s"variable introduced here:")
    }
  }
}

case class UnusedLinearField(vd: Trees#ValDef, override val pos: Position) extends LinearityError(pos) {
  override def report(reporter: Reporter): Unit = {
    reporter.error(pos, s"linear field `${vd.id}` of type `${vd.tpe}` is never used")

    if (vd.getPos != pos) {
      reporter.info(vd.getPos, s"field defined here:")
    }
  }
}

case class NonLinearMethodInvocation(value: Trees#Expr, fi: Trees#FunctionInvocation, fd: Trees#FunDef) extends LinearityError(fi.getPos) {
  override def report(reporter: Reporter): Unit = {
    reporter.error(pos, s"cannot call non-linear method `${fi.id}` on linear value `$value`")
    reporter.info(fd.getPos, s"method defined here:")
  }
}

case class CannotCaptureLinearVar(expr: Trees#Expr, variable: Trees#Variable) extends LinearityError(expr.getPos) {
  override def report(reporter: Reporter): Unit = {
    reporter.error(pos, s"cannot capture linear variable `$variable`")
  }
}

case class DiscardedLinearTerm(expr: Trees#Expr) extends LinearityError(expr.getPos) {
  override def report(reporter: Reporter): Unit = {
    reporter.error(pos, s"linear term cannot be discarded")
  }
}

case class NonLinearTypeContainsLinearFields(adt: Trees#TypedADTDefinition, override val pos: Position) extends LinearityError(pos) {
  override def report(reporter: Reporter): Unit = {
    reporter.error(pos, s"type `${adt.toType}` contains linear fields, and cannot be introduced non-linearly")
    reporter.info(adt.getPos, s"type defined here:")
  }
}

case class CannotInstantiateNonLinearTypeParam(tp: Trees#TypeParameter, ty: Trees#Type) extends LinearityError(ty.getPos) {
  override def report(reporter: Reporter): Unit = {
    reporter.error(pos, s"cannot instantiate type parameter `$tp` with linear type `$ty`")
    reporter.info(tp.getPos, s"type parameter declared here:")
  }
}
