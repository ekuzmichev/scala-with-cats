package ru.ekuzmichev.chapter6

import scala.util.Try

object `6.4 Validated`

object ValidatedTest extends App {
  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._

  type AllErrorsOr[A] = Validated[List[String], A]

  println(
    Semigroupal[AllErrorsOr]
      .product(
        Validated.invalid(List("Error 1", "Error 2")),
        Validated.invalid(List("Error 3"))
      )
  )

  import cats.syntax.validated._

  println(123.valid[List[String]])
  println(List("Badness").invalid[Int])

  import cats.syntax.applicative._      // for pure
  import cats.syntax.applicativeError._ // for raiseError
  type ErrorsOr[A] = Validated[List[String], A]
  println(123.pure[ErrorsOr])
  println(List("Badness").raiseError[ErrorsOr, Int])

  println(Validated.catchOnly[NumberFormatException]("foo".toInt))
  println(Validated.catchNonFatal(sys.error("Badness")))
  println(Validated.fromTry(Try("foo".toInt)))
  println(Validated.fromEither[String, Int](Left("Badness")))
  println(Validated.fromEither[List[String], Int](Left(List("Badness"))))
  println(Validated.fromOption[String, Int](None, "Badness"))

  println(
    32.valid.andThen(a => 42.valid.map(b => a + b))
  )

  import cats.syntax.either._ // for toValidated

  println("Badness".invalid[Int])
  println("Badness".invalid[Int].toEither)
  println("Badness".invalid[Int].toEither.toValidated)
  println("fail".invalid[Int].getOrElse(0))
  println("fail".invalid[Int].fold(_ + "!!!", _.toString))
}
