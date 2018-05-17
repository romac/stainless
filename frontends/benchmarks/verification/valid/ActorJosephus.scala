
import stainless.lang._
import stainless.actors._
import stainless.annotation._
import stainless.collection._

object ActorJosephus {

  case class SoldierB(num: BigInt, next: Option[ActorRef]) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case _ => this
    }
  }

  case class Soldier(num: BigInt) extends ActorRef
  case object Die extends Msg
  case object Act extends Msg
  case class UpdateNext(next: ActorRef) extends Msg

  case object Main extends ActorRef

  @library
  val initialSystem = {
    val behaviors = CMap[ActorRef, Behavior] {
      case Soldier(n) => SoldierB(n, None())
    }

    ActorSystem(behaviors, CMap.withDefault(Nil()))
      .send(Main, Soldier(1), UpdateNext(Soldier(2)))
      .send(Main, Soldier(2), UpdateNext(Soldier(3)))
      .send(Main, Soldier(3), UpdateNext(Soldier(4)))
      .send(Main, Soldier(4), UpdateNext(Soldier(5)))
      .send(Main, Soldier(5), UpdateNext(Soldier(1)))
  }

  def validSoldierNum(num: BigInt) = num >= 1 && num <= 5

  def validSoldier(b: Behavior) = b match {
    case SoldierB(num, None()) => validSoldierNum(num)
    case SoldierB(num, Some(next)) => validSoldierNum(num) && validSoldierRef(num)
    case _ => false
  }

  def validSoldierRef(ref: ActorRef) = ref match {
    case Soldier(num) => validSoldierNum(num)
    case _ => false
  }

  def validRef(ref: ActorRef) = ref == Main || validSoldierRef(ref)

  def validBehaviors(s: ActorSystem) = {
    validSoldier(s.behaviors(Soldier(1))) &&
    validSoldier(s.behaviors(Soldier(2))) &&
    validSoldier(s.behaviors(Soldier(3))) &&
    validSoldier(s.behaviors(Soldier(4))) &&
    validSoldier(s.behaviors(Soldier(5)))
  }

  def validBehaviors_initial = validBehaviors(initialSystem).holds

  def validBehaviors_step(s: ActorSystem, from: ActorRef, to: ActorRef) = {
    require(validMessages(s) && validBehaviors(s) && validRef(from) && validRef(to))
    validBehaviors(s.step(from, to))
  }.holds

  def mainSoldierMsg(msg: Msg) = msg match {
    case UpdateNext(next) => validSoldierRef(next)
    case Act => true
    case _ => false
  }

  def soldierSoldierMsg(msg: Msg) = msg match {
    case UpdateNext(ref) => validSoldierRef(ref)
    case Act => true
    case Die => true
    case _ => false
  }

  def validMessages(s: ActorSystem) = {
    true
  }

  def validMessages_initial = validMessages(initialSystem).holds

  def validMessages_step(s: ActorSystem, from: ActorRef, to: ActorRef) = {
    require(validBehaviors(s) && validMessages(s) && validRef(from) && validRef(to))
    validMessages(s.step(from, to))
  }.holds

  def validSystem(s: ActorSystem) = validMessages(s) && validBehaviors(s)

}

