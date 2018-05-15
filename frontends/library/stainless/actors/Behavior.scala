package stainless.actors

import stainless.lang._
import stainless.collection._
import stainless.annotation._

import scala.language.postfixOps

@library
abstract class Behavior {

  def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior

  @inline
  implicit def sameBehavior: Behavior = this
}

@library
object Behavior {

  case class Stopped() extends Behavior {

    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = {
      Behavior.same
    }

  }

  @inline
  def same(implicit behavior: Behavior): Behavior = behavior

  @inline
  def stopped: Stopped = Stopped()

}
