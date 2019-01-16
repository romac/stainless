import stainless.lang._
import stainless.annotation._

object ContMonad {

  sealed abstract class ===[A, B] {
    @inline def cast(a: A): B
    @inline def flip: B === A
  }

  case class Refl[A]() extends (A === A) {
    @inline override def cast(a: A): A = a
    @inline override def flip: A === A = this
  }

  implicit def refl[A]: A === A = Refl[A]()

  case class Cont[R, A](runCont: (A => R) => R) {
    @inline
    def get(implicit ev: A === R): R = runCont(a => ev.cast(a))

    @inline
    def map[B](f: A => B): Cont[R, B] = Cont {(k: B => R) =>
      runCont((a: A) => k(f(a)))
    }

    @inline
    def flatMap[B](f: A => Cont[R, B]): Cont[R, B] = Cont { (k: B => R) =>
      runCont((a: A) => f(a).runCont(k))
    }
  }

  object Cont {
    // @inline
    def pure[R, A](a: A): Cont[R, A] = cont(k => k(a))

    // @inline
    def whenCond[R](cond: Boolean)(thn: => Cont[R, Unit]): Cont[R, Unit] = {
      if (cond) thn else Cont.pure(())
    }
  }

  @inline
  def cont[R, A](f: (A => R) => R): Cont[R, A] = Cont(f)

  // @inline
  def callCC[R, A, B](f: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] = cont {
    k =>
      f(a => cont(_ => k(a))).runCont(k)
  }

  object FunctorLaws {
    def functorIdentity[R, A](c: Cont[R, A], k: A => R): Boolean = {
      c.map((x: A) => x).runCont(k) == c.runCont(k)
    }.holds

    @library
    def functorAssociativity[R, A, B, C](
        c: Cont[R, A],
        f: A => B,
        g: B => C,
        k: C => R
    ): Boolean = {
      c.map(f).map(g).runCont(k) == c.map(a => g(f(a))).runCont(k)
    }.holds
  }

  case class DivideByZero()

  /*

  def div[R](a: Int, b: Int): Int = {
    if (b == 0) throw DivideByZero()
    a / b
  }

  def test(a: Int, b: Int): Int = {
    val res = try {
      Some(div(a, b))
    } catch {
      case DivideByZero() => None
    }

    res.getOrElse(42)
  }

  */

  @library
  def div[R](a: Int, b: Int, k: DivideByZero => Cont[R, Int]): Cont[R, Int] =
    callCC[R, Int, DivideByZero] { (ok: Int => Cont[R, DivideByZero]) =>
      val c: Cont[R, DivideByZero] = callCC[R, DivideByZero, Unit] {
        (notOk: DivideByZero => Cont[R, Unit]) =>
          val d: Cont[R, Unit] = Cont.whenCond(b == 0)(notOk(DivideByZero()))
          d flatMap { _ =>
            ok(a / b)
          }
      }
      c flatMap { err =>
        k(err)
      }
    }

  def isOk(a: Int, b: Int): Cont[Int, Int] = {
    require(b != 0)
    div(a, b, err => Cont.pure[Int, Int](0))
  } ensuring { _.runCont(a => a) == (a / b) }

  def isNotOk(a: Int, b: Int): Cont[Int, Int] = {
    require(b == 0)
    div(a, b, err => Cont.pure[Int, Int](0))
  } ensuring { _.runCont(a => a) == 0 }

  def testOk = {
    assert(div(10, 2, err => Cont.pure[Int, Int](0)).runCont(a => a) == 5)
  }

  def testNotOk = {
    assert(div(10, 0, err => Cont.pure[Int, Int](0)).runCont(a => a) == 0)
  }
}

