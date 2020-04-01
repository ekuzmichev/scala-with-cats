package ru.ekuzmichev.chapter1

import java.util.Date

object `1.5 Example: Eq` {
  import cats.Eq
  import cats.instances.int._
  import cats.instances.long._
  import cats.syntax.eq._

  val eqInt: Eq[Int] = Eq[Int]

  implicit val dateEq: Eq[Date] =
    Eq.instance[Date]((date1, date2) => date1.getTime === date2.getTime)
}

object ExampleEqTest extends App {
  import `1.5 Example: Eq`._
  import cats.instances.int._
  import cats.instances.option._
  import cats.instances.string._
  import cats.syntax.eq._
  import cats.syntax.option._

  println(eqInt.eqv(1, 1))
  println(1 =!= 1)
  println("hello" === "world")
  println((Some("hello"): Option[String]) === (Some("world"): Option[String]))
  println(Option("hello") === Option.empty[String])
  println("hello".some === "world".some)
  println("hello".some === none)

  val date1 = new Date()
  val date2 = new Date()
  println(date1 === date2)
  println(date1 === date1)
}
