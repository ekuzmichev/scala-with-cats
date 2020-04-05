package ru.ekuzmichev.chapter4

object `4.10 Defining Custom Monads` {
  import cats.Monad

  import scala.annotation.tailrec

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case Some(Right(value)) => Some(value)
      case Some(Left(value))  => tailRecM(value)(f)
      case None               => None
    }
  }
}

object DefiningCustomMonadsTest extends App {
  import `4.10 Defining Custom Monads`._
  import cats.Monad
  import cats.syntax.option._

  println(Monad[Option].flatMap(42.some)(x => 21.some.map(_ + x)))
}
