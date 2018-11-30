import stainless.lang._
import stainless.annotation._

object test {

  sealed abstract class StrList[A] {
    def toString: String = this match {
      case StrCons(head, tail) => head.toString + " :: " + tail.toString
      case StrNil() => "Nil"
    }
    def size: BigInt = this match {
      case StrCons(_, tail) => 1 + tail.size
      case StrNil() => 0
    }
  }
  case class StrCons[A](head: A, tail: StrList[A]) extends StrList[A]
  case class StrNil[A]() extends StrList[A]

  def prop(ls: StrList[BigInt]) = {
    val res = ls.toString
    assert(ls.size < 5)
    assert(StrCons(1, StrCons(2, StrNil())).toString == "1 :: 2 :: Nil")
  }

  // case class HelloWorld[A](foo: BigInt, bar: Boolean, x: A) {
  //   override def toString: String = {
  //     if (bar) "HelloWorld-Foo" else "Foo-HelloWorld"
  //   }
  // }

  // def test(hw: HelloWorld[BigInt]) = {
  //   val res = hw.toString
  //   assert(hw.foo == 42)
  // }
}
