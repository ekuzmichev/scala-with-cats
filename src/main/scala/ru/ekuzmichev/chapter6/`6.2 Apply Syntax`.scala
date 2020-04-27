package ru.ekuzmichev.chapter6

object `6.2 Apply Syntax`

object ApplySyntaxTest extends App {
  import cats.syntax.apply._
  import cats.syntax.option._
  import cats.instances.option._

  println((1.some, 2.some).tupled)
  println((1.some, "Hello".some, false.some).tupled)

  case class Cat(name: String, born: Int, color: String)

  val cat = (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
  ).mapN(Cat.apply)

  println(cat)
}
