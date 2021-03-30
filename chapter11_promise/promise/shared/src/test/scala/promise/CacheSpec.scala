package promise

import zio._
import zio.test.Assertion._
import zio.test._
import zio.test.environment._
import TestAspect._
import scala.util.Random
import java.util.UUID
import zio.duration._

trait Cache[-K, +E, +V] {
  def get(key: K): IO[E, V]
}

object Cache {

  def make[K, R, E, V](lookup: K => ZIO[R, E, V]): URIO[R, Cache[K, E, V]] =
    for {
      r   <- ZIO.environment[R]
      ref <- Ref.make[Map[K, Promise[E, V]]](Map.empty)
    } yield new Cache[K, E, V] {

      def get(key: K): IO[E, V] =
        Promise.make[E, V].flatMap { promise =>
          ref
            .modify { map =>
              map.get(key) match {
                case Some(promise) => (Right(promise), map)
                case None          => (Left(promise), map + (key -> promise))
              }
            }
            .flatMap {
              case Left(promise)  => lookup(key).provide(r).to(promise) *> promise.await
              case Right(promise) => promise.await
            }
        }
    }
}

object CacheSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("Basic promise code")(
      testM("Completing the promise with .complete") {
        for {
          cache      <- Cache.make { x: Int =>
                          ZIO.succeed(UUID.randomUUID)
                        }
          x          <- cache.get(5).fork
          y          <- cache.get(5).fork
          xCompleted <- x.join
          yCompleted <- y.join
        } yield assert(xCompleted)(equalTo(yCompleted))
      } @@ nonFlaky
    )
}
