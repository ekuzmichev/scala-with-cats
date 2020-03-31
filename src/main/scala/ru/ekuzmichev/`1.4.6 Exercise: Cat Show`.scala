package ru.ekuzmichev

object `1.4.6 Exercise: Cat Show` {
  import cats.Show

  case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} years-old ${cat.color} cat")
}

object CatShowTest extends App {
  import `1.4.6 Exercise: Cat Show`._
  import cats.syntax.show._

  val tom = Cat("Tom", 11, "grey")
  println(tom.show)
}
