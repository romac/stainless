import stainless.lang._
import stainless.annotation._

object test {

  sealed abstract class StrList {
    def toString: String = this match {
      case StrCons(head, tail) => head.toString + " :: " + tail.toString
      case StrNil() => "Nil"
    }
    def size: BigInt = this match {
      case StrCons(_, tail) => 1 + tail.size
      case StrNil() => 0
    }
  }
  case class StrCons(head: BigInt, tail: StrList) extends StrList
  case class StrNil() extends StrList

  def bigIntTests = {
    assert(BigInt(42).toString == "42")
    assert(BigInt(-121).toString == "-121")
    assert(BigInt(0).toString == "0")
  }

  def booleanTests = {
    assert(true.toString == "true")
    assert(false.toString == "false")
  }

  def stringTests = {
    assert("Hello".toString == "Hello")
  }

  def intTests = {
    assert(42.toString == "42")
    assert((-121).toString == "-121")
    assert(0.toString == "0")
  }

  def prop(ls: StrList) = {
    val res = ls.toString
    assert(ls.size < 5)
  }

  def prop2 = {
    assert(StrCons(1, StrCons(2, StrNil())).toString == "1 :: 2 :: Nil")
  }

  case class HelloWorld[A](foo: BigInt, bar: Boolean, x: A) {
    override def toString: String = {
      (if (bar) "HelloWorld-" + foo.toString else foo.toString + "-HelloWorld") + ("-" + bar.toString)
    }
  }

  def test(hw: HelloWorld[BigInt]) = {
    assert(hw.toString != "HelloWorld-42-true")
  }
}
