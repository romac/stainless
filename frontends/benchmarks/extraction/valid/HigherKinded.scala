import stainless.lang._

import scala.language.higherKinds

object HigherKinded {

  case class Box[B](b: B)

  def foo[F[_], A](fa: F[A]): F[A] = fa

  def bar(xs: Box[BigInt]): Box[BigInt] = {
    foo[Box, BigInt](xs)
  }
}
