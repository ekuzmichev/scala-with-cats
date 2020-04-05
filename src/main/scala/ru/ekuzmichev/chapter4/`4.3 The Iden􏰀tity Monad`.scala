package ru.ekuzmichev.chapter4

object `4.3 The Iden􏰀tity Monad` {}

object TheIdentityMonadTest extends App {
  import cats.{ Id, Monad }

  val a: Id[Int] = Monad[Id].pure(1)
  println(a)

  val b: Id[Int] = Monad[Id].flatMap(a)(_ + 1)
  println(b)

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val c: Id[Int] = for {
    x <- a
    y <- b
  } yield x + y
  println(c)
}
