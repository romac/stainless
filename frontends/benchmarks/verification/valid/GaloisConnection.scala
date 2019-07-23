// Based on: D. Darais & D. Van Horn, Constructive Galois Connection, JFP 2019

import stainless.lang._
import stainless.lang.StaticChecks._
import stainless.annotation._

object GaloisConnection {

  abstract class Pow[A] {
    def contains(a: A): Boolean
  }

  final case class PowAll[A]() extends Pow[A] {
    def contains(a: A): Boolean = true
  }

  final case class PowSingleton[A](a: A) extends Pow[A] {
    def contains(b: A): Boolean = a == b
  }

  final case class PowSet[A](set: Set[A]) extends Pow[A] {
    def contains(a: A): Boolean = set.contains(a)
  }

  @library
  abstract class PartialOrder[A] {
    def lessThan(a: A, b: A): Boolean

    @law @ghost
    def law_reflexivity(a: A): Boolean = lessThan(a, a)

    @law @ghost
    def law_transitivity(a: A, b: A, c: A): Boolean =
      (lessThan(a, b) && lessThan(b, c)) ==> lessThan(a, c)

    @law @ghost
    def law_antisymmetry(a: A, b: A): Boolean =
      (lessThan(a, b) && lessThan(b, a)) ==> (a == b)
  }

  implicit final class PartialOrderOps[A](val self: A) {
    @inline
    def <=(other: A)(implicit PO: PartialOrder[A]): Boolean = PO.lessThan(self, other)
  }

  object PartialOrder {
    def isMononone[A: PartialOrder, B: PartialOrder](f: A => B, x: A, y: A): Boolean =
      (x <= y) ==> (f(x) <= f(y))
  }

  @library
  abstract class GC[C, A] {
    def extract(c: C): A
    def interpret(a: A): Pow[C]

    implicit val abstractPartialOrder: PartialOrder[A]
    implicit val concretePartialOrder: PartialOrder[C]

    // @law @ghost
    // def law_monotonicity_extract(x: C, y: C): Boolean = {
    //   PartialOrder.isMononone(c => extract(c), x, y)
    // }

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
      interpret(y).contains(x) ==> abstractPartialOrder.lessThan(extract(x), y)
    }

    @ghost
    def soundness(fc: C => C, fa: A => A, x: C, y: A): Boolean = {
      (interpret(y).contains(x) ==> abstractPartialOrder.lessThan(extract(fc(x)), fa(y))) ||
      (interpret(y).contains(x) ==> interpret(fa(y)).contains(fc(x))) ||
      abstractPartialOrder.lessThan(extract(fc(x)), fa(extract(x))) ||
      interpret(fa(extract(x))).contains(fc(x))
    }
  }
}

// object Parity {
//   import GaloisConnection._

//   sealed abstract class Nat {
//     def <=(that: Nat): Boolean = (this, that) match {
//       case (Zero, _)          => true
//       case (Succ(_), Zero)    => false
//       case (Succ(n), Succ(m)) => n <= m
//     }

//     def pred: Nat

//     def isEven: Boolean = this match {
//       case Zero => true
//       case Succ(n) => n.isOdd
//     }

//     def isOdd: Boolean = this match {
//       case Zero => false
//       case Succ(n) => n.isEven
//     }
//   }

//   final case class Succ(pred: Nat) extends Nat
//   final case object Zero extends Nat {
//     val pred: Nat = this
//   }

//   object Nat {
//     def zero: Nat = Zero
//     def succ(n: Nat): Nat = Succ(n)
//   }

//   case object NatPartialOrder extends PartialOrder[Nat] {
//     override def lessThan(@induct a: Nat, b: Nat): Boolean = a <= b

//     @ghost
//     override def law_reflexivity(@induct a: Nat): Boolean = true
//     @ghost
//     override def law_transitivity(@induct a: Nat, b: Nat, c: Nat): Boolean = true
//     @ghost
//     override def law_antisymmetry(@induct a: Nat, b: Nat): Boolean = true
//   }

//   sealed abstract class Parity {
//     def flip: Parity = this match {
//       case Odd => Even
//       case Even => Odd
//     }
//   }

//   object Parity {
//     def flip(p: Parity): Parity = p.flip
//   }

//   final case object Even extends Parity
//   final case object Odd extends Parity

//   def parity(n: Nat): Parity = n match {
//     case Zero => Even
//     case Succ(n) => parity(n).flip
//   }

//   case object ParityPartialOrder extends PartialOrder[Parity] {
//     override def lessThan(a: Parity, b: Parity): Boolean = a == b
//   }

//   case object EvenNats extends Pow[Nat] {
//     def contains(a: Nat) = a.isEven
//   }

//   case object OddNats extends Pow[Nat] {
//     def contains(a: Nat) = a.isOdd
//   }

//   case object ParityGC extends GC[Nat, Parity] {
//     def extract(c: Nat): Parity = parity(c)

//     def interpret(a: Parity): Pow[Nat] = a match {
//       case Even => EvenNats
//       case Odd => OddNats
//     }

//     implicit val concretePartialOrder = NatPartialOrder
//     implicit val abstractPartialOrder = ParityPartialOrder

//     @ghost
//     override def law_expansive(@induct x: Nat): Boolean = {
//       interpret(extract(x)).contains(x)
//     }

//     @ghost
//     override def law_reductive(@induct x: Nat, y: Parity): Boolean = {
//       interpret(y).contains(x) ==> (extract(x) == y)
//     }
//   }

//   @ghost
//   def soundnessTheorem(n: Nat, p: Parity): Boolean = {
//     require(parity(n) == p)
//     ParityGC.soundness(Nat.succ, Parity.flip, n, p)
//   }.holds
// }

object LC {
  import GaloisConnection._

  sealed abstract class Type

  object Type {
    final case object Top                         extends Type
    final case object Bottom                      extends Type
    final case object Bool                        extends Type
    final case class  Arrow(from: Type, to: Type) extends Type

    def isSubTypeOf(a: Type, b: Type): Boolean =
      TypePartialOrder.lessThan(a, b)

    implicit case object TypePartialOrder extends PartialOrder[Type] {
      override def lessThan(a: Type, b: Type): Boolean = (a, b) match {
        case (Bottom, _)  => true
        case (_, Top)     => true
        case (Bool, Bool) => true
        case (Arrow(fa, ta), Arrow(fb, tb)) =>
          lessThan(ta, tb) && lessThan(fb, fa)
        case _ => false
      }
    }
  }

  sealed abstract class Precision
  object Precision {
    final case object Top                                   extends Precision
    final case object Bottom                                extends Precision
    final case object Bool                                  extends Precision
    final case class  Arrow(from: Precision, to: Precision) extends Precision
    final case object Unknown                               extends Precision

    def isMorePrecise(a: Precision, b: Precision): Boolean =
      PrecisionPartialOrder.lessThan(a, b)

    implicit case object PrecisionPartialOrder extends PartialOrder[Precision] {
      override def lessThan(a: Precision, b: Precision): Boolean = (a, b) match {
        case (_, Unknown) =>
          true
        case (Arrow(fa, ta), Arrow(fb, tb)) =>
          lessThan(ta, tb) && lessThan(fa, fb)
        case (a, b) =>
          a == b
      }
    }
  }

  case class PowSubArrow(fromPow: Pow[Type], toPow: Pow[Type]) extends Pow[Type] {
    def contains(tpe: Type): Boolean = tpe match {
      case Type.Arrow(fb, tb) =>
        fromPow.contains(fb) && toPow.contains(tb)
      case _ =>
        false
    }
  }

  case object PrecisionGC extends GC[Type, Precision] {
    implicit val concretePartialOrder = Type.TypePartialOrder
    implicit val abstractPartialOrder = Precision.PrecisionPartialOrder

    def extract(tpe: Type): Precision = tpe match {
      case Type.Bottom        => Precision.Bottom
      case Type.Bool          => Precision.Bottom
      case Type.Top           => Precision.Top
      case Type.Arrow(fa, ta) => Precision.Arrow(extract(fa), extract(ta))
    }

    def interpret(p: Precision): Pow[Type] = p match {
      case Precision.Unknown => PowAll[Type]()
      case Precision.Bottom  => PowSingleton(Type.Bottom)
      case Precision.Bool    => PowSingleton(Type.Bottom)
      case Precision.Top     => PowSingleton(Type.Top)

      case Precision.Arrow(f, t) =>
        PowSubArrow(interpret(f), interpret(t))
    }
  }

  sealed abstract class Term
  final case class Var(name: String)                            extends Term
  final case class Abs(name: String, body: Term)                extends Term
  final case class App(f: Term, x: Term)                        extends Term
  final case class BoolLit(value: Boolean)                      extends Term
  final case class IfThenElse(cnd: Term, thn: Term, els: Term)  extends Term
  final case class Coerce(term: Term, tpe: Type)                extends Term

}

// object WhileLanguage {
//   import GaloisConnection._

//   case object BigIntPartialOrder extends PartialOrder[BigInt] {
//     override def lessThan(a: BigInt, b: BigInt): Boolean = a <= b
//   }

//   sealed abstract class OpArith
//   final case object Plus extends OpArith
//   final case object Minus extends OpArith
//   final case object Times extends OpArith
//   final case object Division extends OpArith

//   sealed abstract class OpComp
//   final case object Equals extends OpComp
//   final case object LessThan extends OpComp

//   sealed abstract class OpBool
//   final case object Or extends OpBool
//   final case object And extends OpBool

//   sealed abstract class AST {
//     def isValue: Boolean = this match {
//       case IntLit(_)  => true
//       case BoolLit(_) => true
//       case UnitLit    => true
//       case _          => false
//     }

//     def toValue: Value = {
//       require(isValue)
//       this match {
//         case IntLit(v)  => IntVal(v)
//         case BoolLit(v) => BoolVal(v)
//         case UnitLit    => UnitVal
//       }
//     }
//   }

//   final case class  IntLit(value: BigInt)                       extends AST
//   final case class  BoolLit(value: Boolean)                     extends AST
//   final case object UnitLit                                     extends AST
//   final case class  Variable(name: String)                      extends AST
//   final case object Rand                                        extends AST
//   final case class  ArithOp(op: OpArith, left: AST, right: AST) extends AST
//   final case class  BoolOp(op: OpBool, left: AST, right: AST)   extends AST
//   final case class  CompOp(op: OpComp, left: AST, right: AST)   extends AST
//   final case class  Seq(a: AST, b: AST)                         extends AST
//   final case class  Assign(variable: String, value: AST)        extends AST
//   final case class  Ite(cnd: AST, thn: AST, els: AST)           extends AST
//   final case class  While(cnd: AST, body: AST)                  extends AST

//   sealed abstract class Value {
//     def toAST: AST = this match {
//       case IntVal(v)  => IntLit(v)
//       case BoolVal(v) => BoolLit(v)
//       case UnitVal    => UnitLit
//     }
//   }
//   final case class  IntVal(value: BigInt)   extends Value
//   final case class  BoolVal(value: Boolean) extends Value
//   final case object UnitVal                 extends Value

//   type Env = Map[String, Value]

//   def step(a: AST)(env: Env): (AST, Env) = a match {
//     case i: IntLit     => (i, env)
//     case b: BoolLit    => (b, env)
//     case u @ UnitLit   => (u, env)
//     case Rand          => (IntLit(42), env) // FIXME

//     case Variable(name) if env contains name =>
//       (env(name).toAST, env)

//     case Variable(name) =>
//       (Variable(name), env)

//     case ArithOp(op, IntLit(l), IntLit(r)) =>
//       op match {
//         case _ => (IntLit(l + r), env) // FIXME
//       }

//     case BoolOp(op, BoolLit(l), BoolLit(r)) =>
//       op match {
//         case _ => (BoolLit(l || r), env) // FIXME
//       }

//     case CompOp(op, l, r) if l.isValue && r.isValue =>
//       op match {
//         case _ => (BoolLit(l == r), env) // FIXME
//       }

//     case Assign(name, value) if value.isValue =>
//       (value, env.updated(name, value.toValue))
//     case Assign(name, value) =>
//       val (next, nextEnv) = step(value)(env)
//       (Assign(name, next), nextEnv)

//     case Seq(a, b) if a.isValue =>
//       (b, env)

//     case Seq(a, b) =>
//       val (next, nextEnv) = step(a)(env)
//       (Seq(next, b), nextEnv)

//     case Ite(cnd, thn, els) if cnd.isValue =>
//       cnd match {
//         case BoolLit(true)  => (thn, env)
//         case BoolLit(false) => (els, env)
//         case _              => (Ite(cnd, thn, els), env)
//       }

//     case Ite(cnd, thn, els) =>
//       val (next, nextEnv) = step(cnd)(env)
//       (Ite(next, thn, els), env)

//     case While(cnd, body) if cnd.isValue =>
//       cnd match {
//         case BoolLit(true)  => (Seq(body, While(cnd, body)), env)
//         case BoolLit(false) => (UnitLit, env)
//         case _              => (While(cnd, body), env)
//       }

//     case While(cnd, body) =>
//       val (next, nextEnv) = step(cnd)(env)
//       (While(next, body), nextEnv)

//     case other => (other, env)
//   }

//   def eval(a: AST)(env: Env): AST = {
//     val (nextA, nextEnv) = step(a)(env)

//     if (nextA == a) nextA
//     else step(nextA)(nextEnv)._1
//   }

//   def test = {
//     val env: Env = Map("x" -> IntVal(1))
//     eval(
//       Seq(
//         While(
//           CompOp(Equals, Variable("x"), IntLit(1)),
//           Assign("x", ArithOp(Plus, Variable("x"), IntLit(1))),
//         ),
//         Variable("x"),
//       )
//     )(env)
//   }

//   sealed abstract class Sign
//   object Sign {
//     final case object None extends Sign
//     final case object Neg  extends Sign
//     final case object Zer  extends Sign
//     final case object Pos  extends Sign
//     final case object NegZ extends Sign
//     final case object NotZ extends Sign
//     final case object PosZ extends Sign
//     final case object Any  extends Sign
//   }

//   case object SignPartialOrder extends PartialOrder[Sign] {
//     import Sign._

//     override def lessThan(a: Sign, b: Sign): Boolean = (a, b) match {
//       case (a, b) if a == b => true
//       case (None, _)        => true
//       case (_, Any)         => true
//       case (Neg, b)         => b == NegZ || b == NotZ
//       case (Zer, b)         => b == NegZ || b == PosZ
//       case (Pos, b)         => b == NotZ || b == PosZ
//       case _                => false
//     }
//   }

//   case object NonePow extends Pow[BigInt] {
//     def contains(n: BigInt) = false
//   }
//   case object NegPow extends Pow[BigInt] {
//     def contains(n: BigInt) = n < 0
//   }
//   case object ZerPow extends Pow[BigInt] {
//     def contains(n: BigInt) = n == 0
//   }
//   case object PosPow extends Pow[BigInt] {
//     def contains(n: BigInt) = n > 0
//   }
//   case object NegZPow extends Pow[BigInt] {
//     def contains(n: BigInt) = n <= 0
//   }
//   case object NotZPow extends Pow[BigInt] {
//     def contains(n: BigInt) = n != 0
//   }
//   case object PosZPow extends Pow[BigInt] {
//     def contains(n: BigInt) = n >= 0
//   }
//   case object AnyPow extends Pow[BigInt] {
//     def contains(n: BigInt) = true
//   }

//   case object IntegerSignGC extends GC[BigInt, Sign] {
//     implicit val concretePartialOrder = BigIntPartialOrder
//     implicit val abstractPartialOrder = SignPartialOrder

//     def extract(b: BigInt): Sign = {
//       if      (b < 0)  Sign.Neg
//       else if (b == 0) Sign.Zer
//       else             Sign.Pos
//     }

//     def interpret(s: Sign): Pow[BigInt] = s match {
//       case Sign.None => NonePow
//       case Sign.Neg  => NegPow
//       case Sign.Zer  => ZerPow
//       case Sign.Pos  => PosPow
//       case Sign.NegZ => NegZPow
//       case Sign.NotZ => NotZPow
//       case Sign.PosZ => PosZPow
//       case Sign.Any  => AnyPow
//     }
//   }
// }
