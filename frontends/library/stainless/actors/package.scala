package stainless

import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

package object actors {

  @library
  abstract class Msg

  @library
  case class Packet(dest: ActorRef, payload: Msg)

  @library
  case class ActorContext(self: ActorRef, var toSend: List[Packet]) {
    def send(to: ActorRef, msg: Msg): Unit = {
      toSend = Packet(to, msg) :: toSend
    }
  }

  @library
  abstract class ActorRef {
    def !(msg: Msg)(implicit ctx: ActorContext): Unit = {
      ctx.send(this, msg)
    }
  }

  def stepPreservesInvariant(inv: ActorSystem => Boolean): Boolean =
    forall { (s: ActorSystem, from: ActorRef, to: ActorRef) =>
      inv(s) ==> inv(s.step(from, to))
    }

}
