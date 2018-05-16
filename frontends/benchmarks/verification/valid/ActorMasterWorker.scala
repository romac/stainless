
import stainless.lang._
import stainless.actors._
import stainless.annotation._
import stainless.collection._

object ActorMasterWorker {

  case class Computation(compute: () => Unit)

  case class MasterB(workersReady: List[ActorRef], pending: List[Computation]) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Master.Ready(worker) if pending.isEmpty =>
        MasterB(Cons(worker, workersReady), pending)

      case Master.Ready(worker) =>
        val (c, rest) = (pending.head, pending.tail)
        worker ! Worker.Order(c)
        MasterB(workersReady, rest)

      case Master.Order(c) if workersReady.isEmpty =>
        MasterB(workersReady, Cons(c, pending))

      case Master.Order(c) =>
        val (worker, rest) = (workersReady.head, workersReady.tail)
        worker ! Worker.Order(c)
        MasterB(rest, pending)

      case _ => this
    }
  }

  case class WorkerB(id: BigInt, master: ActorRef) extends Behavior {
    def processMsg(msg: Msg)(implicit ctx: ActorContext): Behavior = msg match {
      case Worker.Init =>
        master ! Master.Ready(ctx.self)
        this

      case Worker.Order(c) =>
        c.compute()
        master ! Master.Ready(ctx.self)
        this

      case _ => this
    }
  }

  case class Master() extends ActorRef
  object Master {
    case class Order(computation: Computation) extends Msg
    case class Ready(slave: ActorRef) extends Msg
  }

  case class Worker(id: BigInt) extends ActorRef
  object Worker {
    case object Init extends Msg
    case class Order(computation: Computation) extends Msg
  }

  @library
  val initialSystem = {
    val behaviors = CMap[ActorRef, Behavior] {
      case Master()  => MasterB(Nil(), Nil())
      case Worker(n) => WorkerB(n, Master())
    }

    ActorSystem(behaviors, CMap.withDefault(Nil()))
      .send(Master(), Worker(1), Worker.Init)
      .send(Master(), Worker(2), Worker.Init)
      .send(Master(), Worker(3), Worker.Init)
  }

  def validWorker(b: Behavior) = b match {
    case WorkerB(id, Master()) => id == 1 || id == 2 || id == 3
    case _ => false
  }

  def validWorkerId(id: BigInt, b: Behavior) = b match {
    case WorkerB(self, Master()) => id == self
    case _ => false
  }

  def validWorkerRef(ref: ActorRef) = ref match {
    case Worker(id) => id == 1 || id == 2 || id == 3
    case _ => false
  }

  def validMaster(b: Behavior) = b match {
    case MasterB(ready, pending) => ready.forall(validWorkerRef)
    case _ => false
  }

  def validBehaviors(s: ActorSystem) = {
    validMaster(s.behaviors(Master())) &&
    validWorkerId(1, s.behaviors(Worker(1))) &&
    validWorkerId(2, s.behaviors(Worker(2))) &&
    validWorkerId(3, s.behaviors(Worker(3)))
  }

  def validBehaviors_initial = validBehaviors(initialSystem).holds

  def validBehaviors_step(s: ActorSystem, from: ActorRef, to: ActorRef) = {
    require(validMessages(s) && validBehaviors(s) && validRef(from) && validRef(to))
    validBehaviors(s.step(from, to))
  }.holds

  def masterWorkerMsg(msg: Msg) = msg match {
    case Worker.Init => true
    case Worker.Order(_) => true
    case _ => false
  }

  def workerMasterMsg(id: BigInt)(msg: Msg) = msg match {
    case Master.Ready(ref) => Worker(id) == ref
    case _ => false
  }

  def validMessages(s: ActorSystem) = {
    s.inboxes(Master()  -> Master()).isEmpty &&
    s.inboxes(Worker(1) -> Worker(1)).isEmpty &&
    s.inboxes(Worker(1) -> Worker(2)).isEmpty &&
    s.inboxes(Worker(1) -> Worker(3)).isEmpty &&
    s.inboxes(Worker(2) -> Worker(1)).isEmpty &&
    s.inboxes(Worker(2) -> Worker(2)).isEmpty &&
    s.inboxes(Worker(2) -> Worker(3)).isEmpty &&
    s.inboxes(Worker(3) -> Worker(1)).isEmpty &&
    s.inboxes(Worker(3) -> Worker(2)).isEmpty &&
    s.inboxes(Worker(3) -> Worker(3)).isEmpty &&
    s.inboxes(Master() -> Worker(1)).forall(masterWorkerMsg) &&
    s.inboxes(Master() -> Worker(1)).forall(masterWorkerMsg) &&
    s.inboxes(Master() -> Worker(3)).forall(masterWorkerMsg) &&
    s.inboxes(Worker(1) -> Master()).forall(workerMasterMsg(1)) &&
    s.inboxes(Worker(2) -> Master()).forall(workerMasterMsg(2)) &&
    s.inboxes(Worker(3) -> Master()).forall(workerMasterMsg(3))
  }

  def validMessages_initial = validMessages(initialSystem).holds

  def validRef(ref: ActorRef) = ref match {
    case Master() => true
    case Worker(n) => n == 1 || n == 2 || n == 3
    case _ => false
  }

  def validMessages_step(s: ActorSystem, from: ActorRef, to: ActorRef) = {
    require(validBehaviors(s) && validMessages(s) && validRef(from) && validRef(to))
    validMessages(s.step(from, to))
  }.holds

  // object Main {
  //   def run = {
  //     val masterId = Master()
  //     val master = MasterB(Nil(), Nil())

  //     val ids = List(1, 2, 3, 4, 5)
  //     val workerIds = ids.map(Worker(_))
  //     val workers = ids.map(WorkerB(_, masterId))

  //     workerIds.foreach(_ ! Worker.Init)
  //   }
  // }

}
