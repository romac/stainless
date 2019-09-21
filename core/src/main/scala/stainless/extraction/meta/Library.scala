/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package extraction
package meta

trait Library {
  val trees: Trees
  import trees._

  implicit val symbols: trees.Symbols
  implicit val context: inox.Context

  def Api(name: String) =
    symbols.lookup.get[ClassDef](s"stainless.meta.api.$name").get

  def Expr(name: String) =
    Api(s"Expr.$name")

  def ExprClass           = Api("Expr")
  def IntLiteralClass     = Expr("IntLiteral")
  def BooleanLiteralClass = Expr("BooleanLiteral")
  def PlusClass           = Expr("Plus")
  def EqualsClass         = Expr("Equals")
  def AssertClass         = Expr("Assert")
}

object Library {
  def apply(tr: Trees)(syms: tr.Symbols)(implicit ctx: inox.Context): Library {
    val trees: tr.type
  } = new {
    override val trees: tr.type = tr
    override val context = ctx
    override val symbols = syms
  } with Library
}
