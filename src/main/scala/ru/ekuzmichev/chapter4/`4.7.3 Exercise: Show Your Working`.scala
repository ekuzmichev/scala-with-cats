package ru.ekuzmichev.chapter4

import scala.concurrent.Await

object `4.7.3 Exercise: Show Your Working` {
  import cats.data.Writer
  import cats.syntax.writer._

  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialWriter(n: Int): Logged[Int] = {
    import cats.instances.vector._
    import cats.syntax.applicative._
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly(factorialWriter(n - 1).map(_ * n))
      _   <- Vector(s"fact $n $ans").tell
    } yield ans
  }
}

object ShowYourWorkingTest extends App {
  import `4.7.3 Exercise: Show Your Working`._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  import scala.concurrent.duration._
  import scala.language.postfixOps

  println("Test 1")
  factorial(5)
  println()

  println("Test 2")
  Await.result(
    Future.sequence(
      Vector(
        Future(factorial(5)),
        Future(factorial(5))
      )
    ),
    5 seconds
  )
  println()

  println("Test 3")
  Await.result(
    Future.sequence(
      Vector(
        Future(println(factorialWriter(5).run)),
        Future(println(factorialWriter(5).run))
      )
    ),
    5 seconds
  )
}
