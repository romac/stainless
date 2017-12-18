import stainless.lang._
import stainless.annotation._

object linear {

  case class Linear[A](value: A, var used: Boolean) {
    def ! = {
      require(unused)
      used = true
      value
    }

    @inline
    def unused: Boolean = {
      !used
    }
  }

  implicit def delinearize[A](l: Linear[A]): A = {
    require(l.unused)
    l.used = true
    l.value
  }

  implicit def linearize[A](a: A): Linear[A] = {
    Linear(a, false)
  }

}

object sessions {

  import linear._

  @library
  class In[A] {
    @extern
    def ?[B](f: Linear[A] => B): B = {
      ???
    }
  }

  @library
  class Out[A] {

    @extern
    def !(msg: A): Unit = ???

    @extern
    private def create[B](): (In[B], Out[B]) = ???

    @extern
    def !![B](h: Linear[Out[B]] => A): Linear[In[B]] = {
      val (cin, cout) = this.create[B]()
      this ! h(cout)
      cin
    }

    @extern
    def !![B](h: Linear[In[B]] => A): Linear[Out[B]] = {
      val (cin, cout) = this.create[B]()
      this ! h(cin)
      cout
    }

  }

}

import linear._
import sessions._

case class Authenticate(card: String, pin: String, cont: Linear[Out[Response]])

sealed abstract class Response // Authentication response from the ATM
case class Failure()                        extends Response
case class Success(cont: Linear[Out[Menu]]) extends Response

sealed abstract class Menu // Choices available to authenticated user
case class CheckBalance(cont: Linear[Out[Balance]]) extends Menu
case class Quit()                                   extends Menu

case class Balance(amount: BigInt) // User account balance

object protocol {

  def authenticated(card: String, pin: String): Boolean = {
    card == "123456" && pin == "F1"
  }

  def atm(c: Linear[In[Authenticate]]) = {
    c ? {
      case l @ Linear(Authenticate(card, pin, cont), _) if !authenticated(card, pin) =>
        assert(l.unused)
        assert(cont.unused)
        val res = cont ! Failure()
        assert(cont.used)
        assert(l.used)
        res

      case l @ Linear(Authenticate(_, pin, cont), _) =>
        assert(l.unused)
        assert(cont.unused)
        val res = (cont !! (Success(_: Linear[Out[Menu]]))) ? {
          case l @ Linear(CheckBalance(cont), _) =>
            assert(l.unused)
            l.used = true
            assert(!cont.used)
            val res = cont ! Balance(42)
            assert(cont.used)
            assert(l.used)
            res

          case l @ Linear(Quit(), _) => {
            assert(l.unused)
            l.used = true
            val res = ()
            assert(l.used)
            res
          }
        }
        assert(cont.used)
        assert(l.used)
        res
    }
  }

}
