package stainless
package ast

import inox.{Program, Context}
import inox.evaluators.EvaluationResults._

trait ADTPrinter extends Printer {

  val trees: stainless.trees.type
  import trees._

  protected def printADT(adt: ADT)(implicit symbols: Symbols, context: inox.Context): Option[String] = {
    val program = Program(trees)(symbols)
    val evaluator = stainless.evaluators.Evaluator(program, context)

    val adtType @ ADTType(_, tps) = adt.getType
    symbols.lookupCustomToString(adtType)(context) match {
      case None => None
      case Some(printer) =>
        evaluator.eval(printer(adt)) match {
          case Successful(StringLiteral(value)) => Some(value)
          case other => None
        }
    }
  }

  override protected def ppBody(tree: Tree)(implicit ctx: PrinterContext): Unit = tree match {
    case adt: ADT if ctx.opts.symbols.isDefined =>
      printADT(adt)(ctx.opts.symbols.get, Context.empty) match {
        case Some(str) => p"$str"
        case None => super.ppBody(tree)
      }

    case _ => super.ppBody(tree)
  }
}
