import stainless.lang._

object parser {

  sealed abstract class Expr
  case class IntLit(value: BigInt)     extends Expr
  case class BoolLit(value: Boolean)   extends Expr
  case class Plus(a: Expr, b: Expr)    extends Expr
  case class Times(a: Expr, b: Expr)   extends Expr
  case class Equal(a: Expr, b: Expr)   extends Expr
  case class Greater(a: Expr, b: Expr) extends Expr

  def print(expr: Expr): String = expr match {
    case IntLit(value)  => value.toString
    case BoolLit(value) => value.toString
    case Plus(a, b)     => print(a) + " + " + print(b)
    case Times(a, b)    => print(a) + " * " + print(b)
    case Equal(a, b)    => print(a) + " == " + print(b)
    case Greater(a, b)  => print(a) + " > " + print(b)
  }

  val value = Equal(Greater(Plus(IntLit(40), IntLit(2)), IntLit(40)), BoolLit(true))

  // def test = {
  //   assert(print(IntLit(42)) == "42")
  //   assert(print(value) == "40 + 2 > 40 == true")
  // }

  def parse(e: Expr) = {
    assert(print(e) != "40 + 42")
    assert(print(e) != "40 + 42 > 0")
  }

}

