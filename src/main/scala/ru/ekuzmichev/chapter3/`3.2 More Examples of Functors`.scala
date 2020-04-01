package ru.ekuzmichev.chapter3

object `3.2 More Examples of Functors` {}

object MoreExamplesOfFunctorsTest extends App {
  import cats.instances.function._
  import cats.syntax.functor._

  val func1: Int => Double    = _.toDouble
  val func2: Double => Double = _ * 2

  println((func1 map func2)(1))
  println((func1 andThen func2)(1))
  println(func2(func1(1)))
}
