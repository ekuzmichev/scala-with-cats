package ru.ekuzmichev.chapter3

object `3.7 Contravariant and Invariant in Cats`

object ContravariantAndInvariantInCatsTest extends App {
  import cats.instances.string._
  import cats.{ Contravariant, Show }

  val showString = Show[String]

  val showSymbol: Show[Symbol] = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")

  import cats.syntax.contravariant._
  val showInt: Show[Int] = showString.contramap(_.toString)

  println(showSymbol.show(Symbol("dave")))
  println(showInt.show(42))

  import cats.Monoid
  import cats.instances.string._
  import cats.syntax.invariant._
  import cats.syntax.semigroup._

  implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap(Symbol(_))(_.name)
  println(Monoid[Symbol].empty)
  println(Symbol("a") |+| Symbol("few") |+| Symbol("words"))
}
