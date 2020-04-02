package ru.ekuzmichev.chapter3

object `3.2 More Examples of Functors` {}

object MoreExamplesOfFunctorsTest extends App {
  import scala.language.higherKinds
  import cats.Functor
  import cats.instances.function._
  import cats.instances.option._
  import cats.instances.list._
  import cats.syntax.functor._

  val func1: Int => Double    = _.toDouble
  val func2: Double => Double = _ * 2

  println((func1 map func2)(1))
  println((func1 andThen func2)(1))
  println(func2(func1(1)))

  val func: Int => Double                       = _.toDouble
  val liftedFunc: Option[Int] => Option[Double] = Functor[Option].lift(func)

  def square[F[_]: Functor](x: F[Int]): F[Int] = x.map(y => y * y)

  println(square(Option(4)))
  println(square(List(1,2,3)))
}
