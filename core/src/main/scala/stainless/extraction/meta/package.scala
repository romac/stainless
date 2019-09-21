/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction

import scala.language.existentials

package object meta {

  object trees extends meta.Trees with oo.ClassSymbols {
    case class Symbols(
      functions: Map[Identifier, FunDef],
      sorts: Map[Identifier, ADTSort],
      classes: Map[Identifier, ClassDef],
      typeDefs: Map[Identifier, TypeDef],
    ) extends ClassSymbols with AbstractSymbols

    object printer extends Printer {
      val trees: meta.trees.type = meta.trees
    }
  }

  implicit val metaSemantics: inox.SemanticsProvider { val trees: meta.trees.type } =
    new inox.SemanticsProvider {
      val trees: meta.trees.type = meta.trees

      def getSemantics(p: inox.Program { val trees: meta.trees.type }): p.Semantics = new inox.Semantics { self =>
        val trees: p.trees.type = p.trees
        val symbols: p.symbols.type = p.symbols
        val program: Program { val trees: p.trees.type; val symbols: p.symbols.type } =
          p.asInstanceOf[Program { val trees: p.trees.type; val symbols: p.symbols.type }]

        protected def createSolver(ctx: inox.Context): inox.solvers.SolverFactory {
          val program: self.program.type
          type S <: inox.solvers.combinators.TimeoutSolver { val program: self.program.type }
        } = solvers.SolverFactory(self.program.asInstanceOf[StainlessProgram], ctx).asInstanceOf

        protected def createEvaluator(ctx: inox.Context): inox.evaluators.DeterministicEvaluator {
          val program: self.program.type
        } = meta.RecursiveEvaluator(self.program, ctx)
      }.asInstanceOf[p.Semantics] // @nv: unfortunately required here...
    }

  def extractor(implicit ctx: inox.Context) = {
    val lowering = ExtractionPipeline(new CheckingTransformer {
      override val s: trees.type = trees
      override val t: innerclasses.trees.type = innerclasses.trees

      override def transform(fd: s.FunDef): t.FunDef = {
        super.transform(fd.copy(flags = fd.flags.filterNot(_ == s.Meta)))
      }
    })

    utils.DebugPipeline("Meta", Meta(trees)) andThen
    lowering
  }
}
