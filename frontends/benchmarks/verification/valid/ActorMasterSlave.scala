
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

}
