import stainless.lang._
import stainless.annotation._

object implicits {

  case class Foo[A](get: A)

  @keep
  case class ShowFoo[A](showA: Show[A]) extends Show[Foo[A]] {
    override def show(foo: Foo[A]) = "CustomFoo("+ showA.show(foo.get) + ")"
  }

  @keep
  implicit def showFoo[A](implicit showA: Show[A]): Show[Foo[A]] = ShowFoo(showA)

  def prop(foo: Foo[(Boolean, BigInt)]) = {
    assert(foo.get == (true, 42))
  }

  // def prop = {
  //   val foo = ((true, BigInt(42)))
  //   assert(foo.toString == "(true,42)")
  // }

}
