package stainless
package ast

import inox.{Program, Context}
import inox.evaluators.EvaluationResults._

trait ADTPrinter extends Printer {

  val trees: stainless.trees.type
  import trees._

  protected def printADT(adt: ADT)(implicit symbols: Symbols): Option[String] = {
    val program = Program(trees)(symbols)
    val evaluator = stainless.evaluators.Evaluator(program, Context.empty)

    evaluator.eval(ToString(adt)) match {
      case Successful(StringLiteral(value)) => Some(value)
      case other =>
        println(s"Evaluation of $adt.toString failed: $other")
        None
    }
  }

  override protected def ppBody(tree: Tree)(implicit ctx: PrinterContext): Unit = tree match {
    case adt: ADT if ctx.opts.symbols.isDefined =>
      printADT(adt)(ctx.opts.symbols.get) match {
        case Some(str) => p"$str"
        case None => super.ppBody(tree)
      }

    case _ => super.ppBody(tree)
  }
}
