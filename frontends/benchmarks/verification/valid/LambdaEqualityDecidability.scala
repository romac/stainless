import stainless.lang._
import stainless.annotation._

object LambdaEq {

  abstract class Foo {
    @pure
    def bar(f: Int => Int): Int
  }

  val k = (i: Int) => i

  def test1(foo: Foo) = {
    assert(foo.bar(k) == foo.bar(x => k(x)))
  }

  def test2(foo: Foo) = {
    assert(foo.bar(k) != foo.bar(x => k(x)))
  }
}
