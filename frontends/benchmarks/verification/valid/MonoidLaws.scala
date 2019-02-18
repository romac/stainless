
import stainless.lang._
import stainless.proof._
import stainless.annotation._

object MonoidLaws {

  abstract class Monoid[A] {
    def empty: A
    def append(x: A, y: A): A

    @law
    def law_leftIdentity(x: A) = {
      append(empty, x) == x
    }

    @law
    def law_rightIdentity(x: A) = {
      append(x, empty) == x
    }

    @law
    def law_associativity(x: A, y: A, z: A) = {
      append(x, append(y, z)) == append(append(x, y), z)
    }
  }

  def bigIntAdditiveMonoid: Monoid[BigInt] = new Monoid[BigInt] {
    def empty = 0
    def append(x: BigInt, y: BigInt) = x + y
  }

  sealed abstract class Nat {
    def +(m: Nat): Nat = this match {
      case Zero => m
      case Succ(n) => Succ(n + m)
    }

    def *(m: Nat): Nat = this match {
      case Zero => Zero
      case Succ(n) => n * m + m
    }
  }

  final case object Zero extends Nat
  final case class Succ(prev: Nat) extends Nat


  @induct
  def lemma_rightIdentity_plus(x: Nat): Boolean = {
    x + Zero == x
  }.holds

  @induct
  def lemma_associativity_plus(x: Nat, y: Nat, z: Nat): Boolean = {
    x + (y + z) == (x + y) + z
  }.holds

  def natAddMonoid: Monoid[Nat] = new Monoid[Nat] {
    def empty: Nat = Zero
    def append(x: Nat, y: Nat) = x + y

    override def law_rightIdentity(x: Nat) = {
      super.law_rightIdentity(x) because lemma_rightIdentity_plus(x)
    }

    override def law_associativity(x: Nat, y: Nat, z: Nat) = {
      lemma_associativity_plus(x, y, z)
    }
  }
}
