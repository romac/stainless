/* Copyright 2009-2016 EPFL, Lausanne */

package stainless
package extraction
package linearity

abstract class LinearityChecker(val ctx: inox.Context) extends inox.ast.SymbolTransformer { self =>
  val s: Trees
  val t: inlining.Trees

  override def transform(symbols: s.Symbols): t.Symbols = {
    import s._
    import symbols._

    object transformer extends ast.TreeTransformer {
      val s: self.s.type = self.s
      val t: self.t.type = self.t

      override def transform(tpe: s.Type): t.Type = tpe match {
        case s.LinearType(tpe) => transform(tpe)
        case _ => super.transform(tpe)
      }

      override def transform(expr: s.Expr): t.Expr = expr match {
        case s.Linearize(value, _) => transform(value)
        case s.Delinearize(value)  => transform(value)
        case _ => super.transform(expr)
      }

      def transformFun(fd: s.FunDef): t.FunDef = {
        super.transform(fd.copy(flags = fd.flags - Linear))
      }

      def transformADT(adt: s.ADTDefinition): t.ADTDefinition = adt match {
        case sort: s.ADTSort =>
          super.transform(sort.copy(flags = sort.flags - Linear))

        case cons: s.ADTConstructor =>
          super.transform(cons.copy(flags = cons.flags - Linear))
      }
    }

    def check(fd: FunDef): Unit = {
      val typeCheck = TypeChecker(self.s)(symbols)

      typeCheck.check(fd) match {
        case Left(errors) => errors.foreach(_.report(ctx.reporter))
        case Right(()) => ()
      }
    }

    val functions = symbols.functions.values.toSeq
    functions.sortBy(_.getPos).foreach(check)

    t.NoSymbols
      .withADTs(symbols.adts.values.toSeq.map(transformer.transformADT))
      .withFunctions(functions.map(transformer.transformFun))
  }
}
