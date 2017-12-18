
import stainless.lang._
import stainless.annotation._
import stainless.linear._

object prepost {

  @linear
  case class LinVar(value: Option[BigInt]) {

    def put(x: BigInt): Unit = {
      require(isEmpty)
      // send
    }

    def take: BigInt = {
      require(!isEmpty)
      value.get
    }

    def takeOrElse(default: BigInt): BigInt = {
      value.getOrElse(default)
    }

    def isEmpty: Boolean = {
      !value.isDefined
    }
  }

  def ok_1(foo: Linear[LinVar]): BigInt = {
    require(!foo.isEmpty)
    foo.take
  }

  def bad_1_1(foo: Linear[LinVar]): BigInt = {
    require(!foo.isEmpty)
    foo.take + foo.take
  }

  def bad_1_2(foo: Linear[LinVar]): Linear[BigInt] = {
    require(!foo.isEmpty)
    linearize(foo.take + foo.take)
  }

  def positive(x: Linear[BigInt]): Linear[BigInt] = {
    require(x > 0)
    linearize(x * x)
  } ensuring { _ > 0 }

  def abc = {
    positive(BigInt(12))
  } ensuring { _ > 0 }

}
