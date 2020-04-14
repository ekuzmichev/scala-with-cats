package ru.ekuzmichev.chapter6

import cats.Semigroupal
import cats.data.Validated

object `6.4.2 Combining Instances of Validated` {
  import cats.instances.string._ // for Semigroup
  type AllErrorsOr[A] = Validated[String, A]
  Semigroupal[AllErrorsOr]

}

object CombiningInstancesOfValidatedTest extends App {
  import `6.4.2 Combining Instances of Validated`._
  import cats.syntax.apply._ // for tupled
  import cats.syntax.validated._
  import cats.instances.string._ // for Semigroupal
  val res: AllErrorsOr[(Int, Int)] =
    (
      "Error 1".invalid[Int],
      "Error 2".invalid[Int]
    ).tupled
  println(res)

  import cats.instances.vector._

  println(
    (
      Vector(404).invalid[Int],
      Vector(500).invalid[Int]
    ).tupled
  )

  import cats.data.NonEmptyVector

  println(
    (
      NonEmptyVector.of("Error 1").invalid[Int],
      NonEmptyVector.of("Error 2").invalid[Int]
    ).tupled
  )

  import cats.data.NonEmptyList

  println(
    (
      NonEmptyList.of("Error 1").invalid[Int],
      NonEmptyList.of("Error 2").invalid[Int]
    ).tupled
  )

  import cats.data.NonEmptyChain

  println(
    (
      NonEmptyChain.one("Error 1").invalid[Int],
      NonEmptyChain.one("Error 2").invalid[Int]
    ).tupled
  )

  import cats.data.NonEmptySet

  println(
    (
      NonEmptySet.of("Error 1").invalid[Int],
      NonEmptySet.of("Error 2").invalid[Int]
    ).tupled
  )

  import cats.data.NonEmptyMap

  println(
    (
      NonEmptyMap
        .of(
          "e" -> NonEmptyList.of("Error 1"),
          "a" -> NonEmptyList.of("Error 1", "Error 2")
        )
        .invalid[Int],
      NonEmptyMap.of("e" -> NonEmptyList.of("Error 2")).invalid[Int]
    ).tupled
  )
}
