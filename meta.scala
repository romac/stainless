
import stainless.lang._
import stainless.meta.api._
import stainless.annotation._

object Meta {

  // @meta
  // def double(expr: Expr[Int]): Expr[Int] = {
  //   Expr.Plus(expr, expr)
  // }

  // def testDouble0 = {
  //   val two: Int = double(1)
  //   assert(1 + 1 == two)
  // }

  // def testDouble1 = {
  //   val two: Int = splice(double(quote(1)))
  //   assert(1 + 1 == two)
  // }

  // def testDouble2(x: Int) = {
  //   val two: Int = splice(double(quote(x)))
  //   assert(x * 2 == two)
  // }

  @meta
  def concat(expr: Expr[String]): Expr[String] = {
    Expr.StringConcat(expr, expr)
  }

  def testConcat = {
    assert("hellohello" == splice(concat("hello")))
  }

  // @meta
  // def mul(n: Int, expr: Expr[Int]): Expr[Int] = {
  //   require(n >= 0)
  //   if (n == 0) Expr.IntLiteral(0)
  //   else if (n == 1) expr
  //   else Expr.Plus(expr, mul(n - 1, expr))
  // }

  // def testMul = {
  //   val five = splice(mul(5, quote(1)))
  //   assert(5 == five)
  // }

}
