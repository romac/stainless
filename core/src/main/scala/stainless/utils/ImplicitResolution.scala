/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package utils

trait ImplicitResolution {
  val trees: ast.Trees
  val symbols: trees.Symbols
  import trees._

  private[this] implicit val syms = symbols

  private[this] lazy val implicits = symbols.functions.values
    .filter(_.flags contains Implicit)
    .map(fd => fd.returnType -> fd)
    .toMap

  def findImplicitValueForType(tpe: Type): Option[Expr] = {
    println(s"Looking for implicit $tpe")
    val key = implicits.keys.find { keyTpe =>
      lazy val instantiation = symbols.instantiation(keyTpe, tpe)
      tpe == keyTpe || symbols.isSubtypeOf(tpe, keyTpe) || instantiation.isDefined
    }
    if (!key.isDefined) return None
    val instantiation = symbols.instantiation(key.get, tpe).getOrElse(Map())
    val fd = implicits(key.get)
    val tps = fd.tparams.map(tp => instantiation(tp.tp))
    val tfd = fd.typed(tps)

    println(s"Found $tfd")

    if (!tfd.params.forall(_.flags contains Implicit)) return None

    println(s"$tfd needs implicit args: ${tfd.params}")
    val args = tfd.params.map(vd => findImplicitValueForType(vd.getType))
    println(s"Found args: ${args}")
    None

    if (!args.forall(_.isDefined)) return None

    val realArgs = args.map(_.get)
    println(s"Real args: $realArgs")

    Some(tfd.applied(realArgs))
  }

  def test(): Unit = {
    val optionClass = symbols.lookup.get[ADTSort]("stainless.lang.Option")
    val fooClass = symbols.lookup.get[ADTSort]("implicits.Foo")
    if (!optionClass.isDefined || !fooClass.isDefined) return ()

    val optionBooleanType = ADTType(optionClass.get.id, Seq(BooleanType()))
    val fooOptionBooleanType = ADTType(fooClass.get.id, Seq(optionBooleanType))
    println(implicits)
    println(findImplicitValueForType(fooOptionBooleanType))
  }

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
