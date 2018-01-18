/* Copyright 2009-2017 EPFL, Lausanne */

package stainless
package extraction
package linearity

import inox.utils.{NoPosition, Position}

trait Trees extends inlining.Trees { self =>

  case object Linear extends Flag("linear", Seq())

  case class LinearType(tpe: Type) extends Type

  case class Linearize(expr: Expr, tp: Type) extends Expr with CachingTyped {
    protected def computeType(implicit s: Symbols): Type = {
      LinearType(tp)
    }
  }

  case class Delinearize(expr: Expr) extends Expr with CachingTyped {
    protected def computeType(implicit s: Symbols): Type = expr.getType match {
      case LinearType(tpe) => tpe
      case _ => Untyped
    }
  }

  override def extractFlag(name: String, args: Seq[Any]): Flag = (name, args) match {
    case ("linear", Seq()) => Linear
    case _ => super.extractFlag(name, args)
  }

  override def getDeconstructor(that: inox.ast.Trees): inox.ast.TreeDeconstructor { val s: self.type; val t: that.type } = that match {
    case tree: Trees => new TreeDeconstructor {
      protected val s: self.type = self
      protected val t: tree.type = tree
    }.asInstanceOf[TreeDeconstructor { val s: self.type; val t: that.type }]

    case _ => super.getDeconstructor(that)
  }

  override val exprOps: ExprOps { val trees: Trees.this.type } = new {
    protected val trees: Trees.this.type = Trees.this
  } with ExprOps

}

trait Printer extends inlining.Printer {
  protected val trees: Trees
  import trees._

  override def ppBody(tree: Tree)(implicit ctx: PrinterContext): Unit = tree match {
    case LinearType(tp)      => p"Linear[$tp]"
    case Linearize(expr, tp) => p"linearize($expr)"
    case Delinearize(expr)   => p"$expr!"
    case _                   => super.ppBody(tree)
  }
}

trait TreeDeconstructor extends inlining.TreeDeconstructor {
  protected val s: Trees
  protected val t: Trees

  override def deconstruct(f: s.Flag): DeconstructedFlag = f match {
    case s.Linear => (Seq(), Seq(), Seq(), (_, _, _) => t.Linear)
    case _ => super.deconstruct(f)
  }

  override def deconstruct(tpe: s.Type): DeconstructedType = tpe match {
    case s.LinearType(tp) => (Seq(), Seq(tp), Seq(), (_, tps, _) => t.LinearType(tps.head))
    case _ => super.deconstruct(tpe)
  }

  override def deconstruct(e: s.Expr): DeconstructedExpr = e match {
    case s.Linearize(expr, tp) =>
      (Seq(), Seq(), Seq(expr), Seq(tp), (_, _, es, ts) => t.Linearize(es(0), ts(0)))

    case s.Delinearize(expr) =>
      (Seq(), Seq(), Seq(expr), Seq(), (_, _, es, _) => t.Delinearize(es(0)))

    case _ => super.deconstruct(e)
  }
}

trait ExprOps extends extraction.ExprOps {
  protected val trees: Trees
  import trees._
}
