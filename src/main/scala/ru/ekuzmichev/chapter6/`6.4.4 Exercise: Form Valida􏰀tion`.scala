package ru.ekuzmichev.chapter6

import cats.data.Validated

object `6.4.4 Exercise: Form Valida􏰀tion` {
  import cats.instances.list._
  import cats.syntax.apply._
  import cats.syntax.either._

  case class User(name: String, age: Int)

  type FormData    = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(key: String)(params: Map[String, String]): FailFast[String] =
    params.get(key).toRight(List(s"Missing '$key' field"))

  val getName: FormData => FailFast[String] = getValue("name")

  def parseInt(name: String)(s: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](s.toInt).leftMap(_ => List(s"Value $name is not Int but is $s"))

  def nonBlank(name: String)(s: String): FailFast[String] =
    s.asRight.ensure(List(s"Blank $name value"))(_.trim.nonEmpty)
  def nonNegative(name: String)(i: Int): FailFast[Int] = i.asRight.ensure(List(s"Negative $name value"))(_ > 0)

  def readName(params: Map[String, String]): FailFast[String] =
    for {
      name <- getName(params)
      _    <- nonBlank("name")(name)
    } yield name

  def readAge(params: Map[String, String]): FailFast[Int] =
    for {
      ageStr <- getValue("age")(params)
      age    <- parseInt("age")(ageStr)
      _      <- nonNegative("age")(age)
    } yield age

  def readUser(data: FormData): FailFast[User] =
    readUserV(data).toEither

  def readUserV(data: FormData): FailSlow[User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN(User.apply)
}

object FormValidationTest extends App {
  import `6.4.4 Exercise: Form Valida􏰀tion`._
  println(readUser(Map("name" -> "Bob", "age" -> "21")))
  println(readUser(Map("name" -> "", "age"    -> "21")))
  println(readUser(Map("name" -> "Bob", "age" -> "21")))
  println(readUser(Map("name" -> "Bob", "age" -> "-21")))
  println(readUser(Map("name" -> "", "age"    -> "-21")))

  println()

  println(readUserV(Map("name" -> "Bob", "age" -> "21")))
  println(readUserV(Map("name" -> "", "age"    -> "21")))
  println(readUserV(Map("name" -> "Bob", "age" -> "21")))
  println(readUserV(Map("name" -> "Bob", "age" -> "-21")))
  println(readUserV(Map("name" -> "", "age"    -> "-21")))
}
