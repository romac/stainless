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

  final case class PowPred[A](p: A => Boolean) extends Pow[A] {
    def contains(a: A): Boolean = p(a)
  }

  sealed abstract class Ordering
  case object LessThanEq  extends Ordering
  case object GreaterThan extends Ordering
  case object Unrelated   extends Ordering

  @library
  abstract class PartialOrder[A] {
    def compare(a: A, b: A): Ordering

    final def lessThan(a: A, b: A): Boolean = compare(a, b) == LessThanEq

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

  // object PartialOrder {
  //   def isMononone[A: PartialOrder, B: PartialOrder](f: A => B, x: A, y: A): Boolean =
  //     (x <= y) ==> (f(x) <= f(y))
  // }

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
      interpret(fa(extract(x))).contains(fc(x)) ||
      abstractPartialOrder.lessThan(extract(fc(x)), fa(extract(x))) ||
      (interpret(y).contains(x) ==> abstractPartialOrder.lessThan(extract(fc(x)), fa(y))) ||
      (interpret(y).contains(x) ==> interpret(fa(y)).contains(fc(x)))
    }
  }
}

object Parity {
  import GaloisConnection._

  sealed abstract class Nat {
    def <=(that: Nat): Boolean = (this, that) match {
      case (Zero, _)          => true
      case (Succ(_), Zero)    => false
      case (Succ(n), Succ(m)) => n <= m
    }

    def pred: Nat

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
  final case object Zero extends Nat {
    val pred: Nat = this
  }

  object Nat {
    def zero: Nat = Zero
    def succ(n: Nat): Nat = Succ(n)
  }

  case object NatPartialOrder extends PartialOrder[Nat] {
    override def compare(a: Nat, b: Nat): Ordering =
      if (a <= b) LessThanEq else GreaterThan

    @ghost
    override def law_reflexivity(@induct a: Nat): Boolean = true
    @ghost
    override def law_transitivity(@induct a: Nat, b: Nat, c: Nat): Boolean = true
    @ghost
    override def law_antisymmetry(@induct a: Nat, b: Nat): Boolean = true
  }

  sealed abstract class Parity {
    def flip: Parity = this match {
      case Odd  => Even
      case Even => Odd
    }
  }

  object Parity {
    def succ(p: Parity): Parity = p.flip
  }

  final case object Even extends Parity
  final case object Odd extends Parity

  def parity(n: Nat): Parity = n match {
    case Zero    => Even
    case Succ(n) => parity(n).flip
  }

  case object ParityPartialOrder extends PartialOrder[Parity] {
    override def compare(a: Parity, b: Parity): Ordering =
      if (a == b) LessThanEq else GreaterThan
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
      case Odd  => OddNats
    }

    implicit val concretePartialOrder = NatPartialOrder
    implicit val abstractPartialOrder = ParityPartialOrder

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
  def thm_soundness(n: Nat): Boolean = {
    ParityGC.soundness(Nat.succ, Parity.succ, n, parity(n))
  }.holds
}

// object Gradual {
//   import GaloisConnection._

//   sealed abstract class Term
//   final case class Var(name: String)                           extends Term
//   final case class Abs(name: String, body: Term)               extends Term
//   final case class App(f: Term, x: Term)                       extends Term
//   final case class BoolLit(value: Boolean)                     extends Term
//   final case class IfThenElse(cnd: Term, thn: Term, els: Term) extends Term
//   final case class Coerce(term: Term, tpe: Type)               extends Term

//   sealed abstract class Type
//   object Type {
//     final case object Top                         extends Type
//     final case object Bottom                      extends Type
//     final case object Bool                        extends Type
//     final case class  Arrow(from: Type, to: Type) extends Type

//     def isSubTypeOf(a: Type, b: Type): Boolean =
//       TypePartialOrder.lessThan(a, b)

//     def lub(a: Type, b: Type): Boolean = (a, b) match {
//       case _ if a == b =>
//         a
//       case (Arrow(fa, ta), Arrow(fb, tb)) =>
//         Arrow(glb(fa, fb), lub(ta, tb))
//       case _ =>
//         Top
//     }

//     def glb(a: Type, b: Type): Boolean = (a, b) match {
//       case _ if a == b =>
//         a
//       case (Arrow(fa, ta), Arrow(fb, tb)) =>
//         Arrow(lub(fa, fb), glb(ta, tb))
//       case _ =>
//         Bottom
//     }

//     def typeOf(term: Term)(ctx: TyCtx[Type]): Option[Type] = term match {
//       case Var(name) if ctx.contains(name) => Some(ctx(name))
//       case Var(_) => None()

//       case Abs(name, tp, body) =>
//         typeOf(body)(ctx.updated(name, tp)) map { inner =>
//           Arrow(tp, inner)
//         }

//       case App(f, x) => (typeOf(f)(ctx), typeOf(x)(ctx)) match {
//         case (Arrow(ft, tt), xt) if isSubTypeOf(xt, ft) => Some(tt)
//         case _ => None
//       }

//       case BoolLit(_) => Some(Bool)
//       case IfThenElse(cnd, thn, els) =>
//         typeOf(cnd)(ctx) match {
//           case Some(ct) if isSubTypeOf(ct, Bool) => (thn, els) match {
//             case (Some(tt), Some(et)) => Some(lub(tt, et))
//             case _ => None
//           }
//           case _ => None
//         }

//       case Coerce(term, tpe) =>
//         typeOf(term)(ctx) flatMap {
//           case tt if isSubTypeOf(tt, tpe) => Some(tpe)
//           case _ => None
//         }
//     }

//     implicit case object TypePartialOrder extends PartialOrder[Type] {
//       override def lessThan(a: Type, b: Type): Boolean = (a, b) match {
//         case (Bottom, _)  => true
//         case (_, Top)     => true
//         case (Bool, Bool) => true
//         case (Arrow(fa, ta), Arrow(fb, tb)) =>
//           lessThan(ta, tb) && lessThan(fb, fa)
//         case _ => false
//       }
//     }
//   }

//   case class TyCtx[A](ctx: Map[String, A]) {
//     def apply(k: String) = {
//       require(contains(k))
//       ctx(k)
//     }

//     def contains(k: String): Boolean =
//       ctx.contains(k)

//     def updated(k: String, v: A): TyCtx =
//       TyCtx(map.updated(k, v))
//   }

//   sealed abstract class Gradual
//   object Gradual {
//     final case object Top                               extends Gradual
//     final case object Bottom                            extends Gradual
//     final case object Bool                              extends Gradual
//     final case class  Arrow(from: Gradual, to: Gradual) extends Gradual
//     final case object Unknown                           extends Gradual

//     def lub(a: Gradual, b: Gradual): Boolean = (a, b) match {
//       case _ if a == b => a

//       case (Unknown, _) => Unknown
//       case (_, Unknown) => Unknown

//       case (Arrow(fa, ta), Arrow(fb, tb)) =>
//         Arrow(glb(fa, fb), lub(ta, tb))

//       case _ =>
//         Top
//     }

//     def glb(a: Gradual, b: Gradual): Boolean = (a, b) match {
//       case _ if a == b => a

//       case (Unknown, b) => b
//       case (a, Unknown) => a

//       case (Arrow(fa, ta), Arrow(fb, tb)) =>
//         Arrow(lub(fa, fb), glb(ta, tb))

//       case _ =>
//         Bottom
//     }

//     def typeOf(term: Term)(ctx: TyCtx[Gradual]): Option[Gradual] = term match {
//       case Var(name) if ctx.contains(name) => Some(ctx(name))
//       case Var(_) => None()

//       case Abs(name, tp, body) =>
//         typeOf(body)(ctx.updated(name, tp)) map { inner =>
//           Arrow(tp, inner)
//         }

//       case App(f, x) => (typeOf(f)(ctx), typeOf(x)(ctx)) match {
//         case (Arrow(ft, tt), xt) if isSubTypeOf(xt, ft) => Some(tt)
//         case _ => None
//       }

//       case BoolLit(_) => Some(Bool)
//       case IfThenElse(cnd, thn, els) =>
//         typeOf(cnd)(ctx) match {
//           case Some(Bool) => (thn, els) match {
//             case (Some(tt), Some(et)) => Some(lub(tt, et))
//             case _ => None
//           }
//           case _ => None
//         }

//       case Coerce(term, tpe) =>
//         typeOf(term)(ctx) flatMap {
//           case tt if isSubTypeOf(tt, tpe) => Some(tpe)
//           case _ => None
//         }
//     }

//     def isSubTypeOf(a: Gradual, b: Gradual): Boolean =
//       GradualPartialOrder.lessThan(a, b)

//     implicit case object GradualPartialOrder extends PartialOrder[Gradual] {
//       override def lessThan(a: Gradual, b: Gradual): Boolean = (a, b) match {
//         case (Unknown, _) =>
//           true
//         case (_, Unknown) =>
//           true
//         case (Arrow(fa, ta), Arrow(fb, tb)) =>
//           lessThan(ta, tb) && lessThan(fa, fb)
//         case (a, b) =>
//           a == b
//       }
//     }
//   }

//   case class PowSubArrow(fromPow: Pow[Type], toPow: Pow[Type]) extends Pow[Type] {
//     def contains(tpe: Type): Boolean = tpe match {
//       case Type.Arrow(fb, tb) =>
//         fromPow.contains(fb) && toPow.contains(tb)
//       case _ =>
//         false
//     }
//   }

//   case object GradualGC extends GC[Type, Gradual] {
//     implicit val concretePartialOrder = Type.TypePartialOrder
//     implicit val abstractPartialOrder = Gradual.GradualPartialOrder

//     def extract(tpe: Type): Gradual = tpe match {
//       case Type.Bottom        => Gradual.Bottom
//       case Type.Bool          => Gradual.Bottom
//       case Type.Top           => Gradual.Top
//       case Type.Arrow(fa, ta) => Gradual.Arrow(extract(fa), extract(ta))
//     }

//     def interpret(p: Gradual): Pow[Type] = p match {
//       case Gradual.Unknown => PowAll[Type]()
//       case Gradual.Bottom  => PowSingleton(Type.Bottom)
//       case Gradual.Bool    => PowSingleton(Type.Bottom)
//       case Gradual.Top     => PowSingleton(Type.Top)

//       case Gradual.Arrow(f, t) =>
//         PowSubArrow(interpret(f), interpret(t))
//     }
//   }

//   def thm_fat(e: Term): Boolean = {
//     Type.typeOf(e)(TyCtx.empty[Type]).isDefined ==
//     Gradual.typeOf(e)(TyCtx.empty[Gradual]).isDefined
//   }

//   def thm_edl(e: Term): Boolean = {
//     require(e.isClosed)
//     val embedded = Gradual.embed(e)
//     Gradual.typeOf(embedded)(TyCtx.empty[Gradual]) == Some(Gradual.Unknown)
//   }

//   // def thm_gg // TODO
// }

