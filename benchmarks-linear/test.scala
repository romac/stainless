
import stainless.lang._
import stainless.annotation._
import stainless.linear._

object lin {

  @linear
  case class LinVar(value: Option[BigInt]) {

    def put(x: BigInt): Unit = {
      require(isEmpty)
      // send
    }

    def take: BigInt = {
      // require(!isEmpty)
      value.get
    }

    def takeOrElse(default: BigInt): BigInt = {
      value.getOrElse(default)
    }

    def isEmpty: Boolean = {
      !value.isDefined
    }
  }

  def ok0(foo: Linear[LinVar]): Boolean = {
    foo.isEmpty
  }

  def ok1(foo: Linear[LinVar]): Boolean = {
    val bar = foo
    bar.isEmpty
  }

  def ok2(foo: Linear[LinVar]): Unit = {
    foo.put(42)
  }

  def ok3(foo: Linear[LinVar], cond: Boolean): BigInt = {
    if (cond) {
      foo.take
    } else {
      foo.put(42)
      42
    }
  }

  def ok4(foo: Linear[LinVar]): Boolean = {
    foo.isEmpty
  }

  def bad0(foo: Linear[LinVar]): Boolean = {
    true
  }

  def bad1(foo: Linear[LinVar]): Boolean = {
    val bar: Linear[LinVar] = foo
    true
  }

  def bad2(foo: Linear[LinVar]): Boolean = {
    (foo.isEmpty || !foo.isEmpty) == true
  }

  def bad3(foo: Linear[LinVar], cond: Boolean): BigInt = {
    foo.put(42)

    if (cond) {
      42
    } else {
      foo.take
    }
  }

  // ok
  def bad4(foo: Linear[LinVar]): BigInt = {
    if (foo.isEmpty) {
      foo.put(42)
      foo.take
    } else {
      foo.take
    }
  }

  def bad5(foo: LinVar): BigInt = {
    if (foo.isEmpty) {
      foo.put(42)
      foo.take
    } else {
      foo.take
    }
  }

  def bad6(foo: Linear[LinVar]): BigInt = {
    if (foo.isEmpty) {
      foo.put(42)
      foo.put(84)
      84
    } else {
      foo.take
    }
  }

}
