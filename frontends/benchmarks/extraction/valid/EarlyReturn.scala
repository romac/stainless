import stainless.lang._

object EarlyReturn {

  def bar0(x: BigInt): BigInt = {
    if (x < 0) {
      val y = -x
      return y
    }

    x
  } ensuring { _ >= 0 }

  // def bar1(x: BigInt): BigInt = {
  //   if (x < 0) {
  //     val y = -x
  //     return y
  //   } else {
  //     ()
  //   }

  //   x
  // } ensuring { _ >= 0 }

  // def bar2(x: BigInt): BigInt = {
  //   if (x < 0) {
  //     val y = -x
  //     return y
  //   } else {
  //     ()
  //   }

  //   x
  // } ensuring { _ >= 0 }

}
