package ru.ekuzmichev.chapter3

object `3.6.1.1 Exercise: Showing off with Contramap` {
  trait Printable[A] {
    self =>

    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] = (value: B) => self.format(func(value))
  }

  object Printable {
    def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)
    def apply[A](implicit instance: Printable[A]): Printable[A]       = instance

    implicit val stringPrintable: Printable[String]            = (value: String) => "\"" + value + "\""
    implicit val booleanPrintable: Printable[Boolean]          = (value: Boolean) => if (value) "yes" else "no"
    implicit def boxPrintable[A: Printable]: Printable[Box[A]] = (box: Box[A]) => Printable[A].format(box.value)
  }

  final case class Box[A](value: A)
}

object ShowingOffWithContramapTest extends App {
  import `3.6.1.1 Exercise: Showing off with Contramap`._

  println(Printable.format("Hello"))
  println(Printable.format(true))
  println(Printable.format(Box("Hello")))
  println(Printable.format(Box(true)))
}
