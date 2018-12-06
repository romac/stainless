import stainless.lang._
import stainless.annotation._

object implicits {

  case class Foo[A](get: A)

  @library implicit def intFoo: Foo[Int] = Foo(42)
  @library implicit def unitFoo: Foo[Unit] = Foo(())
  @library implicit def complexFoo(implicit intFoo: Foo[Int]): Foo[Boolean] = Foo(intFoo.get > 0)
  @library implicit def optionFoo[A](implicit aFoo: Foo[A]): Foo[Option[A]] =
    Foo(Some(aFoo.get))

  @library val unit = ()

  def prop = {
    assert(intFoo.get == 42)
    assert(unitFoo.get == unit)
    assert(complexFoo.get == true)
    assert(optionFoo[Boolean].get == Some(true))
  }

}
