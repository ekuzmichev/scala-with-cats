package ru.ekuzmichev.chapter6

object `6.3 Semigroupal Applied to Different Types` {}

object SemigroupalAppliedToDifferentTypesTest extends App {
  import cats.Semigroupal
  import cats.instances.future._
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val futurePair =
    Semigroupal[Future]
      .product(
        Future { println("Future 1"); "Hello" },
        Future { println("Future 2"); 123 }
      )
  println(Await.result(futurePair, 1.second))

  import cats.syntax.apply._

  case class Cat(
    name: String,
    yearOfBirth: Int,
    favoriteFoods: List[String]
  )
  val futureCat = (Future("Garfield"), Future(1978), Future(List("Lasagne"))).mapN(Cat.apply)
  println(Await.result(futureCat, 1.second))

  import cats.instances.list._
  println(Semigroupal[List].product(List(1, 2), List(3, 4))) // kind of cartesian product
  println(List(1, 2).zip(List(3, 4)))

  import cats.instances.either._ // for Semigroupal

  type ErrorOr[A] = Either[Vector[String], A]

  println(Semigroupal[ErrorOr].product(Left(Vector("Error 1")), Left(Vector("Error 2"))))
}
