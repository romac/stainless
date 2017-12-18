
import stainless.lang._
import stainless.annotation._
import stainless.linear._

object adt {

  case class Box[A](get: A)

  case class LinStuff[A](stuff: Linear[A])

  case class HalfLin(linear: Linear[BigInt], nonLinear: Boolean)

  case class TwoLin(a: Linear[BigInt], b: Linear[BigInt])

//   @extern
//   def cond(): Boolean = ???

//   def ifElse(foo: Linear[HalfLin]): BigInt = {
//     if (cond()) {
//       foo.linear
//     } else {
//       42
//     }
//   }

//   def closureOk(foo: Linear[BigInt]): Linear[BigInt => BigInt] = {
//     (x: BigInt) => foo + x
//   }

//   def closureBad(foo: Linear[BigInt]): BigInt => BigInt = {
//     (x: BigInt) => foo + x
//   }

//   def doubleUse(box: Linear[Box[BigInt]]): BigInt = {
//     box.get + box.get
//   }

  def noInsideUse(lin: Linear[LinStuff[BigInt]]): BigInt = lin! match {
    case LinStuff(inside) => 42
  }

  def insideUseBad(lin: LinStuff[BigInt]): BigInt = lin match {
    case LinStuff(inside) => inside + inside
  }

  def insideUseBad2(lin: Linear[LinStuff[BigInt]]): BigInt = {
    lin.stuff + lin.stuff
  }

  def insideUseOk(lin: Linear[LinStuff[BigInt]]): BigInt = lin! match {
    case LinStuff(inside) => inside + 42
  }

  def noUse(lin: Linear[HalfLin]): BigInt = {
    if (lin.nonLinear) 42 else 0
  }

  def wrong(bar: Linear[BigInt]): BigInt = {
    bar.! + bar.!
  }

  def noUse2(lin: HalfLin): BigInt = {
    if (lin.nonLinear) 42 else 0
  }

  def twoLinBadIntro1(two: TwoLin): BigInt = {
    42
  }

  def twoLinBadIntro2(two: TwoLin): BigInt = {
    two.a + two.b
  }

  def twoLinBad1(two: Linear[TwoLin]): BigInt = {
    two.a + two.a
  }

  def twoLinBad2(two: Linear[TwoLin]): BigInt = {
    two.b
  }

  def twoLinBad3(two: Linear[TwoLin]): BigInt = {
    two.b
  }

  def linTwoLinOk(two: Linear[TwoLin]): BigInt = two! match {
    case TwoLin(a, b) => a + b
  }

  def linTwoLinBad1(two: Linear[TwoLin]): BigInt = two! match {
    case TwoLin(a, b) => a
  }

  def linTwoLinBad2(two: Linear[TwoLin]): BigInt = two! match {
    case TwoLin(_, b) => b
  }

  def linTwoLinBad3(two: Linear[TwoLin]): BigInt = two! match {
    case TwoLin(_, _) => 42
  }

}
