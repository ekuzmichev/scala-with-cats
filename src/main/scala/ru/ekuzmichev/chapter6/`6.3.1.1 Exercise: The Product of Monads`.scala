package ru.ekuzmichev.chapter6

import scala.language.higherKinds

object `6.3.1.1 Exercise: The Product of Monads` {
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      xx <- x
      yy <- y
    } yield (xx, yy)
}

object TheProductOfMonadsTest extends App {
  import cats.instances.list._
  import cats.instances.either._
  import cats.syntax.either._
  import `6.3.1.1 Exercise: The Product of Monads`._

  println(product(List(1, 2), List(3, 4)))
  println(product(1.asRight, 2.asRight))
  println(product("Error 1".asLeft, "Error 2".asLeft)) // Fail-fast =O
}
