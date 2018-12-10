/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package utils

import scala.collection.mutable.{Map => MutableMap}

object DebugSectionImplicits extends inox.DebugSection("implicits")

trait ImplicitResolution {
  val trees: ast.Trees
  val symbols: trees.Symbols
  import trees._

  private[this] implicit val syms = symbols
  private[this] implicit val debugSection = DebugSectionImplicits

  private[this] lazy val implicits = symbols.functions.values
    .filter(_.flags contains Implicit)
    .map(fd => fd.returnType -> fd)
    .toMap

  private[this] val cache = MutableMap[Type, Option[Expr]]()

  def findImplicitValueForType(tpe: Type)(implicit ctx: inox.Context): Option[Expr] = cache.getOrElseUpdate(tpe, {
    implicit val popts = PrinterOptions.fromContext(ctx)

    ctx.reporter.debug(s"Searching for implicit value of type: ${tpe.asString}")

    val key = implicits.keys.find { keyTpe =>
      lazy val instantiation = symbols.instantiation(keyTpe, tpe)
      tpe == keyTpe || symbols.isSubtypeOf(tpe, keyTpe) || instantiation.isDefined
    }

    if (!key.isDefined) {
      ctx.reporter.debug(s" => FAIL: No matching implicit definition found")
      return None
    }

    val instantiation = symbols.instantiation(key.get, tpe).getOrElse(Map())
    val fd = implicits(key.get)
    val tps = fd.tparams.map(tp => instantiation(tp.tp))
    val tfd = fd.typed(tps)

    if (!tfd.params.forall(_.flags contains Implicit)) {
      ctx.reporter.debug(s" => FAIL: Candidate ${tfd.id.asString} has non-implicit arguments")
      return None
    }

    ctx.reporter.debug(s" => SUCCESS: Found candidate: ${tfd.asString}")

    if (tfd.params.nonEmpty) {
      ctx.reporter.debug(s"Candidate ${tfd.id.asString} needs implicit parameters: " +
        s"${tfd.params.map(_.getType.asString).mkString(", ")}")
    }

    val args = tfd.params.map(vd => vd -> findImplicitValueForType(vd.getType))

    if (!args.forall(_._2.isDefined)) {
      val failed = args.filter(_._2.isEmpty).map(_._1)
      ctx.reporter.debug(s" => FAIL: No evidence found for parameters: ${failed.map(_.getType.asString).mkString(", ")}")
      return None
    }

    val realArgs = args.map(_._2.get)
    val evidence = tfd.applied(realArgs)
    ctx.reporter.debug(s" => SUCCESS: Found implicit invocation: ${evidence.asString}")
    Some(evidence)
  })

//   def test(ctx: inox.Context): Unit = {
//     val optionClass = symbols.lookup.get[ADTSort]("stainless.lang.Option")
//     val fooClass = symbols.lookup.get[ADTSort]("implicits.Foo")
//     if (!optionClass.isDefined || !fooClass.isDefined) return ()

//     val optionBooleanType = ADTType(optionClass.get.id, Seq(BooleanType()))
//     val fooOptionBooleanType = ADTType(fooClass.get.id, Seq(optionBooleanType))
//     println(implicits)
//     println(findImplicitValueForType(fooOptionBooleanType)(ctx))
//   }

}

object ImplicitResolution {
  def apply(tr: ast.Trees)(syms: tr.Symbols): ImplicitResolution {
    val trees: tr.type
    val symbols: syms.type
  } = new {
    val trees: tr.type = tr
    val symbols: syms.type = syms
  } with ImplicitResolution
}
