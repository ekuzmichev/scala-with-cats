package ru.ekuzmichev.chapter4

object `4.2.3 Monad Syntax` {
  import scala.language.higherKinds
  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def sumSquare[F[_]: Monad](x: F[Int], y: F[Int]): F[Int] =
    for {
      a <- x
      b <- y
    } yield a * a + b * b
}

object MonadSyntaxTest extends App {
  import `4.2.3 Monad Syntax`._
  import cats.instances.option._
  import cats.Id

  println(sumSquare(Option(3), Option(4)))
  println(sumSquare(3 : Id[Int], 4 : Id[Int]))

}
