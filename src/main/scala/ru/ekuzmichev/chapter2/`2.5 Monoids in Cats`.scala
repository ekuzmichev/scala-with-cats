package ru.ekuzmichev.chapter2

import cats.kernel.Semigroup

object `2.5 Monoids in Cats`

object MonoidsInCatsTest extends App {
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.option._
  import cats.syntax.semigroup._

  println(Monoid[Option[Int]].combine(None, None))
  println(Monoid[Option[Int]].combine(None, Some(42)))
  println(Monoid[Option[Int]].combine(Some(42), None))
  println(Monoid[Option[Int]].combine(Some(42), Some(42)))

  println(42.some |+| 42.some)
  println(42.some |+| none)

  import cats.instances.function._

  println(Semigroup[Int => Int].combine(_ + 2, Semigroup[Int => Int].combine(_ + 1, _ * 10)).apply(6))
  println(Semigroup[Int => Int].combine(Semigroup[Int => Int].combine(_ + 2, _ + 1), _ * 10).apply(6))
}
