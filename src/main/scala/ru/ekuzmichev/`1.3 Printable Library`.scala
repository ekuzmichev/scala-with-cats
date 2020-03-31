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
  }
}

object Test extends App {
  import `1.3 Printable Library`._
  import PrintableInstances._

  Printable.print("Hello")
  Printable.print(1)
}
