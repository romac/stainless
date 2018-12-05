
import stainless.lang._
import stainless.collection._

object setlist {

  // def test(set: Set[BigInt]) = {
  //   // assert(set.toList != List(BigInt(1), BigInt(2), BigInt(3)))
  //   // assert(set.toString == "Set(1,2,3)")
  // }

  def showTest = {
    assert(Show[(BigInt, Boolean)].show((42, true)) == "(42,true)")
  }

  def showTest(x: (BigInt, Boolean)) = {
    assert(Show[(BigInt, Boolean)].show(x) != "(42,true)")
  }

  def showList(xs: List[BigInt]) = {
    assert(Show[List[BigInt]].show(xs) != "1 :: 2 :: Nil()")
  }
}
