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
