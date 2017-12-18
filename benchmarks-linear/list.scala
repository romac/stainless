
import stainless.lang._
import stainless.annotation._
import stainless.linear._

object list {

  case class -*>[A, B](f: Linear[A] => B) {
    def apply(x: Linear[A]): B = f(x)
  }

  implicit def toLinFun[A, B](f: Linear[A] => B): A -*> B = {
    -*>(f)
  }

  implicit def fromLinFun[A, B](f: A -*> B): Linear[A] => B = {
    f.f
  }

  case class Box[A](a: A) {
    def duplicate = (this, this)
  }

  def boxTest[A](box: Linear[Box[A]]) = {
    box.duplicate
  }

  object LList {
    def nil[A]: LList[A] = LNil[A]()
  }

  sealed abstract class LList[A] {
    def map[B](f: A -*> B): Linear[LList[B]] = this match {
      case LNil()      => LList.nil[B]
      case LCons(h, t) => f(h) :*: t.map(f)
    }

    def :*:(h: Linear[A]): Linear[LList[A]] = {
      LCons(h, this).asInstanceOf[LList[A]]
    }
  }

  case class LCons[A](
    head: Linear[A],
    tail: Linear[LList[A]]
  ) extends LList[A]

  case class LNil[A]() extends LList[A]

}
