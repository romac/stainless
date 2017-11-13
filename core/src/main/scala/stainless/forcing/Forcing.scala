/* Copyright 2009-2017 EPFL, Lausanne */

package stainless
package forcing

import inox.solvers.PurityOptions
import inox.evaluators.EvaluationResults._

object DebugSectionForcing extends inox.DebugSection("forcing")

class Forcing(
  override val sourceProgram: StainlessProgram,
  val context: inox.Context
) extends inox.ast.ProgramTransformer { self =>

  import context._
  import sourceProgram._
  import sourceProgram.trees._
  import sourceProgram.symbols._
  import sourceProgram.trees.exprOps._

  override final object encoder extends IdentityTreeTransformer
  override final object decoder extends IdentityTreeTransformer

  implicit val debugSection = DebugSectionForcing

  implicit private val semantics = sourceProgram.getSemantics

  override final lazy val targetProgram: Program { val trees: sourceProgram.trees.type } = {
    inox.Program(sourceProgram.trees)(transform(sourceProgram.symbols))
  }

  object transformer extends IdentityTreeTransformer

  private def transform(syms: Symbols): Symbols = {
    NoSymbols
      .withADTs(symbols.adts.values.map(transformer.transform).toSeq)
      .withFunctions(symbols.functions.values.toSeq.map { fd =>
        transformer.transform(force(fd))
      })
  }

  private def force(fd: FunDef): FunDef = {
    if (!fd.flags.contains(Force)) return fd

    val empty = NoTree(fd.returnType)
    val checkedOpt = PurityOptions.AssumeChecked

    val (pre, body, post) = breakDownSpecs(fd.fullBody)

    reporter.debug(s"Forcing function ${fd.id} @ ${fd.getPos}...")

    val forced = body match {
      case Some(b) =>
        reporter.debug(s" - Before: $b...")
        val res = simplifyGround(b, force = true)(semantics, context, checkedOpt)
        reporter.debug(s" - After: $res")
        Some(res)
      case None =>
        reporter.error(s"Cannot force an empty tree @ ${fd.getPos}")
        None
    }

    val forcedBody = reconstructSpecs(pre, forced, post, fd.returnType)
    fd.copy(fullBody = forcedBody, flags = fd.flags - Force)
  }
}
