/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package transformers

import scala.language.existentials
import scala.concurrent.duration._
import scala.collection.mutable.{Map => MutableMap}

import inox.{Context, Semantics}
import inox.utils._
import inox.solvers._
import inox.solvers.SolverResponses._
import inox.evaluators.EvaluationResults

trait PartialEvaluator extends SimplifierWithPC { self =>
  import trees._
  import symbols.{simplifier => _, _}
  import exprOps._
  import dsl._

  override protected def simplify(e: Expr, path: Env): (Expr, Boolean) = e match {
    case Annotated(expr, Seq(Unchecked)) =>
      simplify(expr, path)

    case Let(vd, v, b) =>
      simplify(v, path) match {
        case (rv, true) => simplify(replaceFromSymbols(Map(vd -> rv), b), path)
        case _ => super.simplify(e, path)
      }

    case StringConcat(l, r) =>
      (simplify(l, path), simplify(r, path)) match {
        case ((StringLiteral(a), pa), (StringLiteral(b), pb)) =>
          (StringLiteral(a ++ b).copiedFrom(e), pa && pb)

        case ((a, pa), (b, pb)) =>
          (StringConcat(a, b).copiedFrom(e), pa && pb)
      }

    case StringLength(a) =>
      simplify(a, path) match {
        case (StringLiteral(a), pa) =>
          (IntegerLiteral(a.length).copiedFrom(e), pa)
        case (a, pa) =>
          (StringLength(a).copiedFrom(e), pa)
      }

    case SubString(a, start, end) =>
      (simplify(a, path), simplify(start, path), simplify(end, path)) match {
        case ((StringLiteral(a), pa), (IntegerLiteral(start), ps), (IntegerLiteral(end), pe)) =>
          (StringLiteral(a.substring(start.toInt, end.toInt)).copiedFrom(e), pa && ps && pe)
        case ((a, pa), (start, ps), (end, pe)) =>
          (SubString(a, start, end).copiedFrom(e), pa && ps && pe)
      }

    case MapApply(map, key) =>
      (simplify(map, path), simplify(key, path)) match {
        case ((FiniteMap(pairs, dflt, _, _), pm), (key, pk)) =>
          val value = pairs.toMap.getOrElse(key, dflt).copiedFrom(e)
          val (res, pr) = simplify(value, path)
          (res, pr && pm && pk)

        case ((map, pm), (key, pk)) =>
          (MapApply(map, key).copiedFrom(e), pm && pk)
      }

    case MapUpdated(map, key, value) =>
      (simplify(map, path), simplify(key, path), simplify(value, path)) match {
        case ((FiniteMap(pairs, dflt, kT, vT), pm), (key, pk), (value, pv)) =>
          (finiteMap((pairs.toMap + (key -> value)).toSeq, dflt, kT, vT), pm && pk && pv)

        case ((map, pm), (key, pk), (value, pv)) =>
          (MapUpdated(map, key, value).copiedFrom(e), pm && pk && pv)
      }

    case ADTSelector(expr, sel) =>
      simplify(expr, path) match {
        case (adt @ ADT(id, tps, args), pa) if adt.getConstructor.fields.exists(_.id == sel) =>
          val (res, pr) = simplify(args(adt.getConstructor.definition.selectorID2Index(sel)), path)
          (res, pa && pr)

        case (adt, pa) =>
          (ADTSelector(adt, sel).copiedFrom(e), pa)
      }

    case fi @ FunctionInvocation(id, tps, args) =>
      val tfd = fi.tfd
      val (rargs, pargs) = args.map(simplify(_, path)).unzip

      val inlined: Option[Expr] = {
        val (specs, body) = deconstructSpecs(tfd.fullBody)

        body.map { body =>
          val pre = specs.collectFirst { case Precondition(e) => e }.get
          val l @ Lambda(Seq(res), post) = specs.collectFirst { case Postcondition(e) => e }.get

          val newBody: Expr = Assert(pre, Some("Inlined precondition of " + tfd.id.name), Let(res, body,
            Assert(post, Some("Inlined postcondition of " + tfd.id.name), res.toVariable).copiedFrom(l)
          ).copiedFrom(body)).copiedFrom(pre)

          freshenLocals((tfd.params zip rargs).foldRight(newBody) {
            case ((vd, e), body) => Let(vd, e, body).copiedFrom(body)
          })
        }
      }

      def containsChoose(expr: Expr): Boolean = exists {
        case (_: Choose) | (_: NoTree) => true
        case _ => false
      } (expr)

      def isProductiveUnfolding(inlined: Expr): Boolean = {
        def isKnown(expr: Expr): Boolean = expr match {
          case BooleanLiteral(_) => true
          case _ => false
        }

        // Do not unfold the function being considered when simplifying the path condition
        dynBlocked.set(dynBlocked.get + id)

        // Collect all (transitive) recursive calls, and the path condition at that point
        val invocationPaths = collectWithPC(inlined) {
          case (fi: FunctionInvocation, subPath) if transitivelyCalls(fi.id, id) =>
            transform(subPath.toClause, path)
        }

        // Unblock the current function
        dynBlocked.set(dynBlocked.get - id)

        val isProductive = if (tfd.fd.flags contains Synthetic) {
          // If we can decide weather at least one recursive call will be reached or not,
          // we consider the current unfolding to be productive.
          // Note: We don't need to consider all recursive calls,
          // because we know that synthetic functions invocations always terminate.
          invocationPaths.exists(isKnown)
        } else {
          // If we can decide whether or not every single recursive call will be reached or not,
          // we consider the current unfolding to be productive.
          invocationPaths.forall(isKnown)
        }

        isProductive
      }

      def unfold(inlined: Expr): (Expr, Boolean) = {
        // Block further unfoldings function being unfolded when simplifying the result of the unfolding
        dynSteps.set(dynSteps.get + (id -> (dynSteps.get()(id) - 1)))
        val res = simplify(inlined, path)
        // Unblock the function
        dynSteps.set(dynSteps.get + (id -> (dynSteps.get()(id) + 1)))
        res
      }

      inlined
        .filter(_ => isUnfoldable(id)) // Only unfold unblocked functions
        .filter(!containsChoose(_))    // Only unfold functions which do not contain chooses
        .filter(isProductiveUnfolding) // Only unfold when it is productive, see above.
        .map(unfold)
        .getOrElse (
          FunctionInvocation(id, tps, rargs).copiedFrom(fi),
          pargs.foldLeft(isPureFunction(id))(_ && _)
        ) // If the unfolding fails, keep the function invocation, with its simplified arguments

    case _ => super.simplify(e, path)
  }

  protected val maxUnfoldingSteps: Int = 50

  private[this] val dynBlocked = new ThreadLocal[Set[Identifier]] { override def initialValue = Set.empty }

  private[this] val dynSteps = new ThreadLocal[Map[Identifier, Int]] {
    override def initialValue = Map.empty.withDefault(_ => maxUnfoldingSteps)
  }

  private[this] def isUnfoldable(id: Identifier): Boolean = !dynBlocked.get()(id) && (dynSteps.get()(id) > 0)

  private[this] def finiteMap(els: Iterable[(Expr, Expr)], default: Expr, from: Type, to: Type): FiniteMap = {
    FiniteMap(els.toMap.toSeq.filter { case (_, value) => value != default }.sortBy(_._1.toString), default, from, to)
  }
}

