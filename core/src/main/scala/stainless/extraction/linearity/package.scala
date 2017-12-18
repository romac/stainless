/* Copyright 2009-2017 EPFL, Lausanne */

package stainless
package extraction

package object linearity {

  object trees extends Trees with inox.ast.SimpleSymbols {
    case class Symbols(
      functions: Map[Identifier, FunDef],
      adts: Map[Identifier, ADTDefinition]
    ) extends SimpleSymbols with AbstractSymbols

    object printer extends Printer { val trees: linearity.trees.type = linearity.trees }
  }

  def checker(ctx: inox.Context): LinearityChecker {
    val s: trees.type
    val t: inlining.trees.type
  } = new LinearityChecker(ctx) {
    val s: trees.type = trees
    val t: inlining.trees.type = inlining.trees
  }
}
