package promise

import zio._
import zio.test.Assertion._
import zio.test._
import zio.test.environment._
import TestAspect._
import scala.util.Random

object Helpers {

  val randomInt: UIO[Int] =
    UIO.effectTotal(Random.nextInt())
}

object PromiseBasicSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("Basic promise code")(
      // testM("Die") {
      //   for {
      //     promise <- Promise.make[Nothing, Unit]
      //     last    <- (promise.await *> console.putStr("2")).fork
      //     _       <- (console.putStr("1")) *> promise.die(new RuntimeException("Killed the promise!")).fork
      //     _       <- last.join
      //     output  <- TestConsole.output
      //   } yield assert(output)(equalTo(Vector("1", "2")))
      // },
      testM("First") {
        for {
          promise <- Promise.make[Nothing, Unit]
          last    <- (promise.await *> console.putStr("2")).fork
          _       <- (console.putStr("1")) *> promise.succeed(()).fork
          _       <- last.join
          output  <- TestConsole.output
        } yield assert(output)(equalTo(Vector("1", "2")))
      } @@ nonFlaky,
      testM("Complete") {
        for {
          p <- Promise.make[Nothing, Int]
          _ <- p.complete(Helpers.randomInt)
          l <- p.await
          r <- p.await
        } yield assert(l)(equalTo(r))
      },
      testM("CompleteWith") {
        for {
          p <- Promise.make[Nothing, Int]
          _ <- p.completeWith(Helpers.randomInt)
          l <- p.await
          r <- p.await
        } yield assert(l)(not(equalTo(r)))
      }
    )

  //   // suite("Promise - die")(
  //   //   testM("First") {
  //   //     for {
  //   //       promise <- Promise.make[Nothing, Unit]
  //   //       last    <- (promise.await *> console.putStr("2")).fork
  //   //       _       <- (console.putStr("1")) *> promise.die(new RuntimeException("Killed the promise!")).fork
  //   //       _       <- last.join
  //   //       output  <- TestConsole.output
  //   //     } yield assert(output)(equalTo(Vector("1", "2")))
  //   //   }
  //   // )

  //   suite("Complete")(
  //     testM("Complete") {
  //       for {
  //         p <- Promise.make[Nothing, Int]
  //         _ <- p.complete(Helpers.randomInt)
  //         l <- p.await
  //         r <- p.await
  //       } yield assert(l)(equalTo(r))
  //     }
  //   )
  //   suite("CompleteWith")(
  //     testM("CompleteWith") {
  //       for {
  //         p <- Promise.make[Nothing, Int]
  //         _ <- p.completeWith(Helpers.randomInt)
  //         l <- p.await
  //         r <- p.await
  //       } yield assert(l)(not(equalTo(r)))
  //     }
  //   )
  // } @@ jvmOnly

}
