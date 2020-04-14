package ru.ekuzmichev.chapter6

object `6.2.1 Fancy Functors and Apply Syntax` {
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.invariant._
  import cats.instances.list._
  import cats.instances.string._
  import cats.syntax.apply._

  final case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply
  val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat] =
    (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)
}

object FancyFunctorsAndApplySyntaxTest extends App {
  import `6.2.1 Fancy Functors and Apply Syntax`._
  import cats.syntax.semigroup._

  val garfield = Cat("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

  println(garfield |+| heathcliff)
}
