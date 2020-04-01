package ru.ekuzmichev.chapter1

import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.util.Date

object `1.4 Meet Cats` {

  import cats.Show
  import cats.instances.int._
  import cats.instances.string._

  val showInt: Show[Int]       = Show[Int]
  val showString: Show[String] = Show[String]

  implicit val dateShow: Show[Date]                   = (date: Date) => s"${date.getTime} ms since the epoch."
  implicit val localDateShow: Show[LocalDate]         = Show.fromToString
  implicit val localDateTimeShow: Show[LocalDateTime] = Show.show(dt => s"${dt.toEpochSecond(ZoneOffset.UTC)} seconds")
}

object MeetCatsTest extends App {
  import `1.4 Meet Cats`._
  import cats.syntax.show._

  println(showInt.show(42))
  println(showString.show("Hello"))
  println(new Date().show)
  println(LocalDate.now().show)
  println(LocalDateTime.now().show)
}
