/* Copyright 2009-2016 EPFL, Lausanne */

package stainless

import org.scalatest._

import scala.util.{Try, Success, Failure}

class ExtractionSuite extends FunSpec with inox.ResourceUtils with InputUtils {

  private def assertSuccess[A](result: Try[A])(implicit reporter: inox.TestSilentReporter): Unit = {
      result match {
        case Success(_) =>
          assert(true)

        case Failure(ex) =>
          println("====== ERROR ======")
          ex.printStackTrace()
          assert(false)
      }
  }

  private def extractOne(file: java.io.File): Unit = {
    implicit val reporter = new inox.TestSilentReporter

    val ctx = inox.Context(reporter, new inox.utils.InterruptManager(reporter))

    val tryProgram = Try(loadFiles(ctx, Seq(file.getPath))._2)

    it("should be successful") { assertSuccess(tryProgram) }

    if (tryProgram.isSuccess) {
      val program = tryProgram.get

      it("should typecheck") {
        program.symbols.ensureWellFormed
        for (fd <- program.symbols.functions.values.toSeq) {
          import program.symbols._
          assert(isSubtypeOf(fd.fullBody.getType, fd.returnType))
        }
      }

      val component = stainless.verification.VerificationComponent
      val tryExProgram = Try(component.extract(program, ctx))

      describe("and transformation") {
        it("should be successful") { assertSuccess(tryExProgram) }

        if (tryExProgram.isSuccess) {
          val exProgram = tryExProgram.get
          it("should produce no errors") { assert(reporter.lastErrors.isEmpty) }

          it("should typecheck") {
            exProgram.symbols.ensureWellFormed
            for (fd <- exProgram.symbols.functions.values.toSeq) {
              import exProgram.symbols._
              assert(isSubtypeOf(fd.fullBody.getType, fd.returnType))
            }
          }

          it("should typecheck without matches") {
            for (fd <- exProgram.symbols.functions.values.toSeq) {
              import exProgram.symbols._
              assert(isSubtypeOf(matchToIfThenElse(fd.fullBody).getType, fd.returnType))
            }
          }
        }
      }
    }
  }

  private def testAll(dir: String): Unit = {
    val fs = resourceFiles(dir, _.endsWith(".scala")).toList

    describe(s"Program extraction in '$dir/'") {
      fs.foreach { file =>
        describe(s"Extraction of '$dir/${file.getName}'") {
          extractOne(file)
        }
      }
    }
  }

  testAll("extraction")
  testAll("verification/valid")
  testAll("verification/invalid")
  testAll("verification/unchecked")
  testAll("imperative/valid")
  testAll("imperative/invalid")
  testAll("termination/valid")
  testAll("termination/looping")
}
