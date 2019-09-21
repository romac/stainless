/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package meta

trait Trees extends innerclasses.Trees { self =>

  case object Meta extends Flag("meta", Seq.empty)

  case class Splice(metaExpr: Expr, tpe: Type) extends Expr {
    def getType(implicit s: Symbols) = tpe
  }

  case class Quote(expr: Expr) extends Expr {
    def getType(implicit s: Symbols) = {
      s.lookup.get[ClassDef]("stainless.meta.api.Expr").get.typed.toType
    }
  }

  override def extractFlag(name: String, args: Seq[Expr]): Flag = (name, args) match {
    case ("meta", Seq()) => Meta
    case _ => super.extractFlag(name, args)
  }

  override val exprOps: ExprOps { val trees: Trees.this.type } = new {
    protected val trees: Trees.this.type = Trees.this
  } with ExprOps

  override def getDeconstructor(that: inox.ast.Trees): inox.ast.TreeDeconstructor { val s: self.type; val t: that.type } = that match {
    case tree: Trees => new TreeDeconstructor {
      protected val s: self.type = self
      protected val t: tree.type = tree
    }.asInstanceOf[TreeDeconstructor { val s: self.type; val t: that.type }]

    case _ => super.getDeconstructor(that)
  }
}

trait Printer extends innerclasses.Printer {
  protected val trees: Trees
  import trees._

  override def ppBody(tree: Tree)(implicit ctx: PrinterContext): Unit = tree match {
    case Splice(metaExpr, tpe) =>
      p"splice[$tpe]($metaExpr)"

    case Quote(expr) =>
      p"quote($expr)"

    case _ => super.ppBody(tree)
  }
}

trait GhostTraverser extends innerclasses.GhostTraverser {
  val trees: Trees
}

trait ExprOps extends innerclasses.ExprOps {
  protected val trees: Trees
}

trait TreeDeconstructor extends innerclasses.TreeDeconstructor {
  protected val s: Trees
  protected val t: Trees

  override def deconstruct(f: s.Flag): DeconstructedFlag = f match {
    case s.Meta => (Seq(), Seq(), Seq(), (_, _, _) => t.Meta)
    case _ => super.deconstruct(f)
  }

  override def deconstruct(e: s.Expr): Deconstructed[t.Expr] = e match {
    case s.Splice(e, tpe) =>
      (Seq(), Seq(), Seq(e), Seq(tpe), Seq(), (_, _, es, tps, _) => t.Splice(es.head, tps.head))

    case s.Quote(e) =>
      (Seq(), Seq(), Seq(e), Seq(), Seq(), (_, _, es, tps, _) => t.Quote(es.head))

    case _ => super.deconstruct(e)
  }
}
