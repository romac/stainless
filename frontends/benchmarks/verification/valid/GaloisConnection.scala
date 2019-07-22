// Based on: D. Darais & D. Van Horn, Constructive Galois Connection, JFP 2019

import stainless.lang._
import stainless.lang.StaticChecks._
import stainless.annotation._

object GaloisConnection {

  @library
  abstract class Pow[A] {
    def contains(a: A): Boolean
  }

  @library
  abstract class Poset[A] {
    // TODO: Laws
    def lessThan(a: A, b: A): Boolean
  }

  @library
  abstract class GC[C, A] {
    def extract(c: C): A
    def interpret(a: A): Pow[C]

    val poset: Poset[A]

    @law @ghost
    def law_correspondence(x: C, y: A): Boolean = {
      law_expansive(x) && law_reductive(x, y)
    }

    @law @ghost
    def law_expansive(x: C): Boolean = {
      interpret(extract(x)).contains(x)
    }

    @law @ghost
    def law_reductive(x: C, y: A): Boolean = {
      interpret(y).contains(x) ==> poset.lessThan(extract(x), y)
    }

    @ghost
    def soundness(fc: C => C, fa: A => A, x: C, y: A): Boolean = {
      (interpret(y).contains(x) ==> poset.lessThan(extract(fc(x)), fa(y))) ||
      (interpret(y).contains(x) ==> interpret(fa(y)).contains(fc(x))) ||
      poset.lessThan(extract(fc(x)), fa(extract(x))) ||
      interpret(fa(extract(x))).contains(fc(x))
    }
  }
}

object Parity {
  import GaloisConnection._

  sealed abstract class Nat {
    def isEven: Boolean = this match {
      case Zero => true
      case Succ(n) => n.isOdd
    }

    def isOdd: Boolean = this match {
      case Zero => false
      case Succ(n) => n.isEven
    }
  }
  final case class Succ(pred: Nat) extends Nat
  final case object Zero extends Nat

  object Nat {
    def zero: Nat = Zero
    def succ(n: Nat): Nat = Succ(n)
  }

  sealed abstract class Parity {
    def flip: Parity = this match {
      case Odd => Even
      case Even => Odd
    }
  }

  final case object Even extends Parity
  final case object Odd extends Parity

  object Parity {
    def succ(p: Parity): Parity = p match {
      case Odd => Even
      case Even => Odd
    }
  }

  def parity(n: Nat): Parity = n match {
    case Zero => Even
    case Succ(n) => parity(n).flip
  }

  case object ParityPoset extends Poset[Parity] {
    override def lessThan(a: Parity, b: Parity): Boolean = {
      a == b
    }
  }

  case object EvenNats extends Pow[Nat] {
    def contains(a: Nat) = a.isEven
  }

  case object OddNats extends Pow[Nat] {
    def contains(a: Nat) = a.isOdd
  }

  case object ParityGC extends GC[Nat, Parity] {
    def extract(c: Nat): Parity = parity(c)

    def interpret(a: Parity): Pow[Nat] = a match {
      case Even => EvenNats
      case Odd => OddNats
    }

    val poset = parityposet

    @ghost
    override def law_expansive(@induct x: Nat): Boolean = {
      interpret(extract(x)).contains(x)
    }

    @ghost
    override def law_reductive(@induct x: Nat, y: Parity): Boolean = {
      interpret(y).contains(x) ==> (extract(x) == y)
    }
  }

  @ghost
  def soundnessTheorem(n: Nat, p: Parity): Boolean = {
    require(parity(n) == p)
    ParityGC.soundness(Nat.succ, Parity.succ, n, p)
  }.holds
}

object WhileLanguage {
  import GaloisConnection._

  sealed abstract class OpArith
  final case object Plus extends OpArith
  final case object Minus extends OpArith
  final case object Times extends OpArith
  final case object Division extends OpArith

  sealed abstract class OpComp
  final case object Equals extends OpComp
  final case object LessThan extends OpComp

  sealed abstract class OpBool
  final case object Or extends OpBool
  final case object And extends OpBool

  sealed abstract class AST
  final case class IntLit(value: BigInt) extends AST
  final case class BoolLit(value: Boolean) extends AST
  final case class Variable(name: String) extends AST
  final case object Rand extends AST
  final case class ArithOp(op: OpArith, left: AST, right: AST) extends AST
  final case class BoolOp(op: OpBool, left: AST, right: AST) extends AST
  final case class CompOp(op: OpComp, left: AST, right: AST) extends AST
  final case object Skip extends AST
  final case class Seq(a: AST, b: AST) extends AST
  final case class Assign(variable: Variable, value: AST) extends AST
  final case class Ite(cnd: AST, thn: AST, els: AST) extends AST
  final case class While(cnd: AST, body: AST) extends AST

  sealed abstract class Sign
  object Sign {
    final case object None extends Sign
    final case object Neg  extends Sign
    final case object Zer  extends Sign
    final case object Pos  extends Sign
    final case object NegZ extends Sign
    final case object NotZ extends Sign
    final case object PosZ extends Sign
    final case object Any  extends Sign
  }

  case object SignPoset extends Poset[Sign] {
    import Sign._

    override def lessThan(a: Sign, b: Sign): Boolean = (a, b) match {
      case (a, b) if a == b => true
      case (None, _)        => true
      case (_, Any)         => true
      case (Neg, b)         => b == NegZ || b == NotZ
      case (Zer, b)         => b == NegZ || b == PosZ
      case (Pos, b)         => b == NotZ || b == PosZ
      case _                => false
    }
  }

  case object NonePow extends Pow[BigInt] {
    def contains(n: BigInt) = false
  }
  case object NegPow extends Pow[BigInt] {
    def contains(n: BigInt) = n < 0
  }
  case object ZerPow extends Pow[BigInt] {
    def contains(n: BigInt) = n == 0
  }
  case object PosPow extends Pow[BigInt] {
    def contains(n: BigInt) = n > 0
  }
  case object NegZPow extends Pow[BigInt] {
    def contains(n: BigInt) = n <= 0
  }
  case object NotZPow extends Pow[BigInt] {
    def contains(n: BigInt) = n != 0
  }
  case object PosZPow extends Pow[BigInt] {
    def contains(n: BigInt) = n >= 0
  }
  case object AnyPow extends Pow[BigInt] {
    def contains(n: BigInt) = true
  }

  case object IntegerSignGC extends GC[BigInt, Sign] {
    val poset = SignPoset

    def extract(b: BigInt): Sign = {
      if      (b < 0)  Sign.Neg
      else if (b == 0) Sign.Zer
      else             Sign.Pos
    }

    def interpret(s: Sign): Pow[BigInt] = s match {
      case Sign.None => NonePow
      case Sign.Neg  => NegPow
      case Sign.Zer  => ZerPow
      case Sign.Pos  => PosPow
      case Sign.NegZ => NegZPow
      case Sign.NotZ => NotZPow
      case Sign.PosZ => PosZPow
      case Sign.Any  => AnyPow
    }
  }
}
