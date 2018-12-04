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

  def booleanTests = {
    assert(true.toString == "true")
    assert(false.toString == "false")
  }

  def stringTests = {
    assert("Hello".toString == "Hello")
  }

  def bigIntTests = {
    assert(BigInt(0).toString == "0")
    assert(BigInt(1).toString == "1")
    assert(BigInt(42).toString == "42")
    assert(BigInt(-121).toString == "-121")
  }

  def longTests = {
    assert((0: Long).toString == "0")
    assert((1: Long).toString == "1")
    assert((42: Long).toString == "42")
    assert((-121: Long).toString == "-121")
  }

  def intTests = {
    assert(0.toString == "0")
    assert(1.toString == "1")
    assert(42.toString == "42")
    assert((-121).toString == "-121")
  }

  def shortTests = {
    assert((0: Short).toString == "0")
    assert((1: Short).toString == "1")
    assert((42: Short).toString == "42")
    assert((-121: Short).toString == "-121")
  }

  def byteTests = {
    assert((0: Byte).toString == "0")
    assert((1: Byte).toString == "1")
    assert((42: Byte).toString == "42")
    assert((-121: Byte).toString == "-121")
  }

  def tupleTests = {
    assert((1, 2).toString == "(1,2)")
    assert((true, "Hello").toString == "(true,Hello)")
    assert((1 -> true, false -> 2).toString == "((1,true),(false,2))")
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
