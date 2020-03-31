package ru.ekuzmichev

object `1.3 Printable Library` {
  trait Printable[A] {
    def format(value: A): String
  }

  object Printable {
    def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)
    def print[A](value: A)(implicit printable: Printable[A]): Unit    = println(format(value))
  }

  object PrintableInstances {
    implicit val stringPrintable: Printable[String] = (s: String) => s
    implicit val intPrintable: Printable[Int]       = (i: Int) => i.toString
    implicit val catPrintable: Printable[Cat]       = (cat: Cat) => s"${cat.name} is a ${cat.age} years-old ${cat.color} cat"
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit printable: Printable[A]): String = printable.format(value)
      def print(implicit printable: Printable[A]): Unit    = println(value.format)
    }
  }

  case class Cat(name: String, age: Int, color: String)

}

object Test extends App {
  import `1.3 Printable Library`._
  import PrintableInstances._
  import PrintableSyntax._

  val string = "Hello"
  Printable.print(string)
  string.print

  val int = 42
  Printable.print(int)
  int.print

  val tom = Cat("Tom", 11, "grey")
  Printable.print(tom)
  tom.print
}
