package ru.ekuzmichev.chapter4

object `4.8.1 Crea􏰀ting and Unpacking Readers` {
  import cats.data.Reader

  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String]    = Reader(_.name)
  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")
  val feedKitty: Reader[Cat, String]  = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed  <- feedKitty
    } yield s"$greet. $feed."
}

object CreatingAndUnpackingReadersTest extends App {
  import `4.8.1 Crea􏰀ting and Unpacking Readers`._

  println(catName.run(Cat("Tom", "meat")))
  println(greetAndFeed.run(Cat("Tom", "meat")))
}
