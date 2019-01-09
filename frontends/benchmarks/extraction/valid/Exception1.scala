import stainless.lang._

object Exception1 {

  case class LessThanZero(value: BigInt) extends Exception
  case class EqualZero() extends Exception

  def foo0(x: BigInt): BigInt = {
    if (x < 0) {
      throw LessThanZero(x)
    }

    if (x == 0) {
      throw EqualZero()
    }

    if (x == 42) return 42

    return -x
  }
}
