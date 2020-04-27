package ru.ekuzmichev.chapter3

object `3.8 Aside: Par􏰀tial Unifica􏰀tion`

object PartialUnificationTest extends App {
  import cats.instances.function._
  import cats.syntax.functor._

  val func1 = (x: Int) => x.toDouble
  val func2 = (y: Double) => y * 2
  val func3 = func1.map(func2)

  println(func3(1))
}
