
import stainless.lang._
import stainless.annotation._

object lin {


  case class LinVar(value: Option[BigInt]) {

    def put(x: BigInt): Unit = {
      // require(isEmpty)
      // send
    }

    def take: BigInt = {
      // require(!isEmpty)
      // value.get
      42
    }

    def takeOrElse(default: BigInt): BigInt = {
      value.getOrElse(default)
    }

    def isEmpty: Boolean = {
      !value.isDefined
    }
  }

  def ok0(foo: Linear[LinVar]): Boolean = {
    require(foo.unused)
    val res = foo.isEmpty
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

  // def ok1(foo: Linear[LinVar]): Boolean = {
  //   require(foo.unused)
  //   val bar = foo
  //   val res = bar.isEmpty
  //   assert(foo.used, "linear variable `foo` must be used at least once")
  //   res
  // }

  def ok2(foo: Linear[LinVar]): Unit = {
    require(foo.unused)
    val res = foo.put(42)
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

  def ok3(foo: Linear[LinVar], cond: Boolean): BigInt = {
    require(foo.unused)
    val res: BigInt = if (cond) {
      foo.take
    } else {
      foo.put(42)
      42
    }
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

  def ok4(foo: Linear[LinVar]): Boolean = {
    require(foo.unused)
    val res = foo.isEmpty
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

  def bad0(foo: Linear[LinVar]): Boolean = {
    require(foo.unused)
    val res = true
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

  // def bad1(foo: Linear[LinVar]): Boolean = {
  //   require(foo.unused)
  //   val bar: Linear[LinVar] = foo
  //   val res = true
  //   assert(foo.used, "linear variable `foo` must be used at least once")
  //   res
  // }

  def bad2(foo: Linear[LinVar]): Boolean = {
    require(foo.unused)
    val res = (foo.isEmpty || !foo.isEmpty) == true
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

  def bad3(foo: Linear[LinVar], cond: Boolean): BigInt = {
    require(foo.unused)
    foo.put(42)

    val res: BigInt = if (cond) {
      42
    } else {
      foo.take
    }
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

  // ok
  def bad4(foo: Linear[LinVar]): BigInt = {
    require(foo.unused)
    val res = if (foo.isEmpty) {
      foo.put(42)
      foo.take
    } else {
      foo.take
    }
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

  def bad5(foo: LinVar): BigInt = {
    require(foo.unused)
    val res = if (foo.isEmpty) {
      foo.put(42)
      foo.take
    } else {
      foo.take
    }
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

  def bad6(foo: Linear[LinVar]): BigInt = {
    require(foo.unused)
    val res: BigInt = if (foo.isEmpty) {
      foo.put(42)
      foo.put(84)
      84
    } else {
      foo.take
    }
    assert(foo.used, "linear variable `foo` must be used at least once")
    res
  }

}
