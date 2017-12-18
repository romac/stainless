
object inst {

  import stainless.lang._
  import stainless.collection._
  import stainless.annotation._
  import stainless.linear._

  def foo(xs: Linear[Option[Linear[BigInt]]]) = {
    bar(xs)
  }

  def bar[A](xs: A): (A, A) = {
    (xs, xs)
  }

  def foo(xs: Linear[Option[Linear[BigInt]]]) = {
    bar(xs)
  }

}
