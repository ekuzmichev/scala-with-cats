package ru.ekuzmichev.chapter6

object `6.1.1 Joining Two Contexts` {}

object JoiningTwoContextsTest extends App {
  import cats.Semigroupal
  import cats.instances.option._
  import cats.syntax.option._

  println(Semigroupal[Option].product(1.some, 2.some))
  println(Semigroupal[Option].product(1.some, none))
  println(Semigroupal.tuple3(1.some, 2.some, 3.some))
  println(Semigroupal.tuple3(1.some, 2.some, none))
  println(Semigroupal.map3(1.some, 2.some, 3.some)(_ + _ + _))
}
